import * as lsp from "vscode-languageserver-types";

interface Projects {
	[projectId: string]: Hovercraft;
}

interface Hovercraft {
	projectId: string;
	pages: Array<Page>;
}

interface Page {
	entries: Array<Entry>;
}

interface Entry {
	document: lsp.TextDocumentIdentifier;
	projectId: string;
	hover: lsp.Hover;
	definition: { uri: lsp.URI, range: lsp.Range };
	moniker: any;
	rootPath: string;
}
