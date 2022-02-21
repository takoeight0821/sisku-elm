import * as lsp from "vscode-languageserver-types";

interface Projects {
	[projectName: string]: Hovercraft;
}

type Hovercraft = Array<Page>;

interface Page {
	document: lsp.TextDocumentIdentifier;
	entries: Array<Entry>;
}

interface Entry {
	hover: lsp.Hover;
	definition: { uri: lsp.URI, range: lsp.Range };
	moniker: any;
	rootPath: string;
}
