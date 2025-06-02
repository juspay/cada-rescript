import subprocess
import json
import re
from collections import defaultdict
from tree_sitter import Language, Parser, Node, Query
import tree_sitter_rescript
import networkx as nx
import hashlib


class DetailedChanges:
    def __init__(self, module_name):
        self.moduleName = module_name
        self.addedFunctions = []
        self.modifiedFunctions = []
        self.deletedFunctions = []
        self.addedTypes = []
        self.modifiedTypes = []
        self.deletedTypes = []
        self.addedExternals = []
        self.modifiedExternals = []
        self.deletedExternals = []

    def __str__(self):
        return (
            f"Module: {self.moduleName}\n"
            f"Added Functions: {self.addedFunctions}\n"
            f"Modified Functions: {self.modifiedFunctions}\n"
            f"Deleted Functions: {self.deletedFunctions}\n"
            f"Added Types: {self.addedTypes}\n"
            f"Modified Types: {self.modifiedTypes}\n"
            f"Deleted Types: {self.deletedTypes}\n"
            f"Added Externals: {self.addedExternals}\n"
            f"Modified Externals: {self.modifiedExternals}\n"
            f"Deleted Externals: {self.deletedExternals}"
        )

def format_rescript_file(file_pth):
    try:
        subprocess.run(["npx", "rescript", "format", file_pth], capture_output=True)
    except:
        pass


class RescriptFileDiff:
    def __init__(self, file1_path: str, file2_path: str):
        language = Language(tree_sitter_rescript.language())
        parser = Parser(language)

        try:
            format_rescript_file(file1_path)
            format_rescript_file(file2_path)
        except:
            pass

        self.old_file_ast = parser.parse(open(file1_path, "rb").read())
        self.old_file_root = self.old_file_ast.root_node

        self.new_file_ast = parser.parse(open(file2_path, "rb").read())
        self.new_file_root = self.new_file_ast.root_node

        self.module_name = file1_path.split("/")[-1]
    

    def get_decl_name(self, node: Node, node_type, name_type):
        for child in node.children:
            if node_type is None and child.type == name_type:
                return child.text.decode()
            if child.type == node_type or node_type is None:
                for binding_child in child.children:
                    if binding_child.is_named and binding_child.type == name_type:
                        return binding_child.text.decode()
        return None

    def extract_components(self, tree):

        cursor = tree.walk()
        stack = [cursor.node]
        functions = {}
        types = {}
        externals = {}

        while stack:
            node = stack.pop()
            if node.type == "let_declaration":
                node_name = self.get_decl_name(node, "let_binding", "value_identifier")
                node_content = node.text.decode()
                if node_name and node_content:
                    hash_body = hashlib.md5(node_content.encode()).hexdigest()
                    functions[node_name] = (hash_body, node_content)
            elif node.type == "type_declaration":
                node_name = self.get_decl_name(node, "type_binding", "type_identifier")
                node_content = node.text.decode()
                if node_name and node_content:
                    hash_body = hashlib.md5(node_content.encode()).hexdigest()
                    types[node_name] = (hash_body, node_content)
            elif node.type == "external_declaration":
                node_name = self.get_decl_name(node, None, "value_identifier")
                node_content = node.text.decode()
                if node_name and node_content:
                    hash_body = hashlib.md5(node_content.encode()).hexdigest()
                    externals[node_name] = (hash_body, node_content)
            else:
                for child in reversed(node.children):
                    stack.append(child)
        return functions, types, externals

    def diff_components(self, funcs_before, funcs_after):
        before_names = set(funcs_before)
        after_names = set(funcs_after)

        added_names = after_names - before_names
        deleted_names = before_names - after_names
        possibly_modified_names = before_names & after_names

        modified = set()
        added = set()
        deleted = set()

        for name in possibly_modified_names:
            if funcs_before[name] != funcs_after[name]:
                modified.add((name, funcs_before[name][1], funcs_after[name][1]))
        
        for name in added_names:
            added.add((name, funcs_after[name][1]))
        
        for name in deleted_names:
            deleted.add((name, funcs_before[name]))

        return {
            "added": list(added),
            "deleted": list(deleted),
            "modified": list(modified),
        }
    
    def process_files(self):
        old_funcs, old_types, old_externals = self.extract_components(self.old_file_root)
        new_funcs, new_types, new_externals = self.extract_components(self.new_file_root)

        changes = DetailedChanges(self.module_name)
        
        funcs_diff = self.diff_components(old_funcs, new_funcs)
        types_diff = self.diff_components(old_types, new_types)
        externals_diff = self.diff_components(old_externals, new_externals)

        changes.addedFunctions, changes.deletedFunctions, changes.modifiedFunctions = funcs_diff.values()
        changes.addedTypes, changes.deletedTypes, changes.modifiedTypes = types_diff.values()
        changes.addedExternals, changes.deletedExternals, changes.modifiedExternals = externals_diff.values()

        return changes

    def exp(self):
        new_funcs, new_types, externals = self.extract_components(self.new_file_root)
        print(externals)

