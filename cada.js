#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { execSync, spawn } = require('child_process');
const { promisify } = require('util');

// Try to import ReScript's parser
let ResParser;
try {
    // Try different possible locations for ReScript parser
    ResParser = require('rescript/lib/js/res_parser.js');
} catch (e1) {
    try {
        ResParser = require('@rescript/core/lib/js/res_parser.js');
    } catch (e2) {
        try {
            ResParser = require('rescript-compiler/lib/js/res_parser.js');
        } catch (e3) {
            console.warn('ReScript parser not found, falling back to CLI parsing');
            ResParser = null;
        }
    }
}

// Data structures matching your Haskell output
class DetailedChanges {
    constructor(moduleName) {
        this.moduleName = moduleName;
        this.addedFunctions = [];
        this.modifiedFunctions = [];
        this.deletedFunctions = [];
        this.addedTypes = [];
        this.modifiedTypes = [];
        this.deletedTypes = [];
        this.addedExternals = [];
        this.modifiedExternals = [];
        this.deletedExternals = [];
    }
}

// Extract module name from file path
function extractModuleName(filepath) {
    return path.basename(filepath, '.res')
        .split(/[-_]/)
        .map(word => word.charAt(0).toUpperCase() + word.slice(1))
        .join('');
}

// Parse ReScript file using ReScript's actual parser
async function parseRescriptFile(filepath) {
    try {
        const content = fs.readFileSync(filepath, 'utf8');
        const moduleName = extractModuleName(filepath);

        if (ResParser) {
            // Use ReScript's native parser
            try {
                const ast = ResParser.parseImplementation(content, filepath);
                const result = extractFromAST(ast);
                return {
                    moduleName,
                    functions: result.functions,
                    types: result.types,
                    externals: result.externals
                };
            } catch (parseError) {
                console.warn(`Parser failed for ${filepath}, falling back to CLI:`, parseError.message);
            }
        }

        // Fallback to CLI parsing
        return await parseViaReScriptCLI(filepath, content, moduleName);

    } catch (error) {
        console.error(`Error parsing ${filepath}:`, error.message);
        return null;
    }
}

// Parse using ReScript CLI and extract declarations
async function parseViaReScriptCLI(filepath, content, moduleName) {
    try {
        // Validate ReScript syntax
        execSync(`rescript format ${filepath}`, { stdio: 'pipe' });

        // Extract declarations using AST-like parsing
        const functions = [];
        const types = [];
        const externals = [];

        const lines = content.split('\n');
        let currentConstruct = null;
        let braceDepth = 0;
        let parenDepth = 0;

        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            const trimmed = line.trim();

            // Skip empty lines and comments
            if (!trimmed || trimmed.startsWith('//') || trimmed.startsWith('/*')) {
                continue;
            }

            // Track brace/paren depth
            braceDepth += (line.match(/\{/g) || []).length - (line.match(/\}/g) || []).length;
            parenDepth += (line.match(/\(/g) || []).length - (line.match(/\)/g) || []).length;

            // Start of a new construct
            if (!currentConstruct) {
                if (trimmed.startsWith('let ')) {
                    const match = trimmed.match(/^let\s+([a-zA-Z_][a-zA-Z0-9_']*)/);
                    if (match) {
                        currentConstruct = {
                            type: 'function',
                            name: match[1],
                            lines: [line],
                            startLine: i
                        };
                    }
                } else if (trimmed.startsWith('type ')) {
                    const match = trimmed.match(/^type\s+([a-zA-Z_][a-zA-Z0-9_']*)/);
                    if (match) {
                        currentConstruct = {
                            type: 'type',
                            name: match[1],
                            lines: [line],
                            startLine: i
                        };
                    }
                } else if (trimmed.startsWith('external ')) {
                    const match = trimmed.match(/^external\s+([a-zA-Z_][a-zA-Z0-9_']*)/);
                    if (match) {
                        currentConstruct = {
                            type: 'external',
                            name: match[1],
                            lines: [line],
                            startLine: i
                        };
                    }
                }
            } else {
                // Continue current construct
                currentConstruct.lines.push(line);
            }

            // End of construct (when we're back to depth 0 and line doesn't end with continuation)
            if (currentConstruct && braceDepth === 0 && parenDepth === 0 &&
                !trimmed.endsWith(',') && !trimmed.endsWith('|') &&
                !trimmed.endsWith('->') && !trimmed.endsWith('=>')) {

                const code = currentConstruct.lines.join('\n');

                switch (currentConstruct.type) {
                    case 'function':
                        functions.push([currentConstruct.name, code]);
                        break;
                    case 'type':
                        types.push([currentConstruct.name, code]);
                        break;
                    case 'external':
                        externals.push([currentConstruct.name, code]);
                        break;
                }

                currentConstruct = null;
            }
        }

        return {
            moduleName,
            functions,
            types,
            externals
        };

    } catch (error) {
        console.error(`CLI parsing failed for ${filepath}:`, error.message);
        return null;
    }
}

// Extract declarations from ReScript AST (when native parser is available)
function extractFromAST(ast) {
    const functions = [];
    const types = [];
    const externals = [];

    // This would need proper AST traversal based on ReScript's AST structure
    // For now, return empty arrays as placeholder
    // In a full implementation, you'd traverse the AST nodes

    return { functions, types, externals };
}

// Compare two modules and return changes
function compareModules(oldModule, newModule) {
    if (!oldModule || !newModule) {
        if (!oldModule && newModule) {
            // Entire module added
            return getEntireModuleAsChanges(newModule, true);
        } else if (oldModule && !newModule) {
            // Entire module deleted
            return getEntireModuleAsChanges(oldModule, false);
        }
        return new DetailedChanges('Unknown');
    }

    const changes = new DetailedChanges(newModule.moduleName);

    // Compare functions
    const oldFuncs = new Map(oldModule.functions);
    const newFuncs = new Map(newModule.functions);

    // Added functions
    for (const [name, code] of newModule.functions) {
        if (!oldFuncs.has(name)) {
            changes.addedFunctions.push([name, code]);
        }
    }

    // Deleted functions
    for (const [name, code] of oldModule.functions) {
        if (!newFuncs.has(name)) {
            changes.deletedFunctions.push([name, code]);
        }
    }

    // Modified functions
    for (const [name, newCode] of newModule.functions) {
        const oldCode = oldFuncs.get(name);
        if (oldCode && oldCode !== newCode) {
            changes.modifiedFunctions.push([name, oldCode, newCode]);
        }
    }

    // Compare types (same logic)
    const oldTypes = new Map(oldModule.types);
    const newTypes = new Map(newModule.types);

    for (const [name, code] of newModule.types) {
        if (!oldTypes.has(name)) {
            changes.addedTypes.push([name, code]);
        }
    }

    for (const [name, code] of oldModule.types) {
        if (!newTypes.has(name)) {
            changes.deletedTypes.push([name, code]);
        }
    }

    for (const [name, newCode] of newModule.types) {
        const oldCode = oldTypes.get(name);
        if (oldCode && oldCode !== newCode) {
            changes.modifiedTypes.push([name, oldCode, newCode]);
        }
    }

    // Compare externals (same logic)
    const oldExternals = new Map(oldModule.externals);
    const newExternals = new Map(newModule.externals);

    for (const [name, code] of newModule.externals) {
        if (!oldExternals.has(name)) {
            changes.addedExternals.push([name, code]);
        }
    }

    for (const [name, code] of oldModule.externals) {
        if (!newExternals.has(name)) {
            changes.deletedExternals.push([name, code]);
        }
    }

    for (const [name, newCode] of newModule.externals) {
        const oldCode = oldExternals.get(name);
        if (oldCode && oldCode !== newCode) {
            changes.modifiedExternals.push([name, oldCode, newCode]);
        }
    }

    return changes;
}

// Get entire module as changes (for new/deleted modules)
function getEntireModuleAsChanges(module, isAdded) {
    const changes = new DetailedChanges(module.moduleName);

    if (isAdded) {
        changes.addedFunctions = module.functions;
        changes.addedTypes = module.types;
        changes.addedExternals = module.externals;
    } else {
        changes.deletedFunctions = module.functions;
        changes.deletedTypes = module.types;
        changes.deletedExternals = module.externals;
    }

    return changes;
}

// Git operations
function cloneRepo(repoUrl, localPath) {
    if (!fs.existsSync(localPath)) {
        console.log(`Cloning repository to ${localPath}...`);
        execSync(`git clone ${repoUrl} ${localPath}`, { stdio: 'inherit' });
    } else {
        console.log(`Repository already exists at ${localPath}`);
    }
}

function getChangedFiles(branchName, newCommit, localPath) {
    const oldCwd = process.cwd();
    process.chdir(localPath);

    try {
        execSync(`git checkout ${branchName}`, { stdio: 'pipe' });
        const commit = execSync(`git rev-parse ${branchName}`, { encoding: 'utf8' }).trim();
        const diff = execSync(`git diff --name-only ${commit} ${newCommit}`, { encoding: 'utf8' });

        const files = diff.split('\n')
            .filter(file => file.trim().length > 0)
            .filter(file => file.endsWith('.res'));

        return files;
    } finally {
        process.chdir(oldCwd);
    }
}

// Main function
async function main() {
    const args = process.argv.slice(2);

    if (args.length !== 5) {
        console.error('Usage: node rescript-differ.js <repo_url> <local_path> <branch_name> <current_commit> <path>');
        process.exit(1);
    }

    const [repoUrl, localRepoPath, branchName, currentCommit, _path] = args;

    try {
        // Clone repository
        cloneRepo(repoUrl, localRepoPath);

        // Get changed files
        console.log('Getting changed files...');
        const changedFiles = getChangedFiles(branchName, currentCommit, localRepoPath);
        console.log(`Found ${changedFiles.length} changed ReScript files`);

        const filePaths = changedFiles.map(file => path.join(localRepoPath, file));

        // Process modules for previous commit
        console.log('Processing modules for previous commit...');
        const previousModules = new Map();
        for (const filepath of filePaths) {
            const result = await parseRescriptFile(filepath);
            if (result) {
                previousModules.set(result.moduleName, result);
            }
        }

        // Switch to current commit
        console.log('Switching to current commit...');
        const oldCwd = process.cwd();
        process.chdir(localRepoPath);
        execSync(`git checkout ${currentCommit}`, { stdio: 'pipe' });
        process.chdir(oldCwd);

        // Process modules for current commit
        console.log('Processing modules for current commit...');
        const currentModules = new Map();
        for (const filepath of filePaths) {
            const result = await parseRescriptFile(filepath);
            if (result) {
                currentModules.set(result.moduleName, result);
            }
        }

        // Compare and generate changes
        console.log('Generating changes...');
        const allChanges = [];

        // Get all unique module names
        const allModuleNames = new Set([...previousModules.keys(), ...currentModules.keys()]);

        for (const moduleName of allModuleNames) {
            const oldModule = previousModules.get(moduleName);
            const newModule = currentModules.get(moduleName);
            const changes = compareModules(oldModule, newModule);
            allChanges.push(changes);
        }

        // Write output
        console.log('Writing output files...');
        fs.writeFileSync('detailed_changes.json', JSON.stringify(allChanges, null, 2));

        console.log(`Processing complete! Found changes in ${allChanges.length} modules.`);
        console.log('Output written to: detailed_changes.json');

    } catch (error) {
        console.error('Error:', error.message);
        process.exit(1);
    }
}

// Run if called directly
if (require.main === module) {
    main().catch(console.error);
}

module.exports = {
    parseRescriptFile,
    compareModules,
    DetailedChanges
};