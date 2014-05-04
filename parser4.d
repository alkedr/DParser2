import std.stdio;
import std.typecons;
import std.algorithm;
import std.conv;
import std.string;
import std.array;



class TokenParser {
	enum Kind {
		IDENTIFIER,
		KEYWORD,
		CHAR_SEQUENCE,
		LIST,
		...
	}
}

// Генерируем все возможные sequence'ы и потом объединяем их
// на лету (в методе code()) в дерево с помощью peek() и pop()

// stack of TokenParser's
// pop для удаления общих элементов в случаях как этот:
// Seq1: A B C
// Seq2: A Y Z
// Если pop выносит identifier, то создаётся временная переменная
class Sequence {
	TokenParser peek() {

	}

	TokenParser pop() {

	}
}

class ParserGenerator {
	TokenParser[][] choiceOfSequences;
}






class ParserGenerator {
	string condition;
	ParserGenerator[] choices;

	void addToSequence(ParserGenerator item) {
		if (choices.length == 0) {
			addToChoice(item);
		} else {
			foreach (choice; choices) {
				choice.addToSequence(item);
			}
		}
	}

	void addToChoice(ParserGenerator item) {
		//foreach (choice; choices) {
		//	if (choice.condition == item.condition) {

		//	}
		//}
		choices ~= item;
	}



	static ParserGenerator sequence(ParserGenerator... items) {
		ParserGenerator result = new ParserGenerator;
		foreach (item; items) {
			result.addToSequence(item);
		}
		return result;
	}

	static ParserGenerator choice(ParserGenerator... items) {
		ParserGenerator result = new ParserGenerator;
		foreach (item; items) {
			result.addToChoice(item);
		}
		return result;
	}

	static ParserGenerator optional(ParserGenerator item) {
		return choice(item, new ParserGenerator);
	}

	static ParserGenerator list(string fieldName, string separator)(ParserGenerator item) {
		return null;
	}

	static ParserGenerator dotList(string fieldName)(ParserGenerator item) {
		return list!(fieldName, ".")(item);
	}

	static ParserGenerator commaList(string fieldName)(ParserGenerator item) {
		return list!(fieldName, ",")(item);
	}

	static ParserGenerator identifier(string fieldName)() {
		return null;
	}

	static ParserGenerator keyword(dstring s)() {
		return null;
	}

}









void parse() {
	// begin node ImportDeclaration
	if (parseKeyword("import")) {
		ImportDeclaration result = new ImportDeclaration;
		do {
			// begin node Import
			Import i = new Import;
			Token aliasOrPackageName = parseIdentifier();
			if (parseOperator("=")) {
				i.aliasName = aliasOrPackageName;
				aliasOrPackageName = parseIdentifier();
			}
			// begin node ModuleName
			i.packageName ~= aliasOrPackageName;
			while (parseOperator(".")) {
				result.packageName ~= parseIdentifier();
			}
			// end node ModuleName
			if (parseOperator(":")) {
				do {
					// begin node ImportBinding
					Token aliasOrSymbol = parseIdentifier();
					if (parseOperator("=")) {
						i.aliasName = aliasOrSymbol;
						i.symbolName = parseIdentifier();
					} else {
						i.symbolName = aliasOrSymbol;
					}
					// end node ImportBinding
				} while (parseOperator(","));
			}
			// end node Import
		} while (parseOperator(","));
		parseOperator(";")
	}
	// end node ImportDeclaration
}




import identifier;
import identifier = identifier;
import identifier = identifier : identifier = identifier;
import identifier = identifier.identifier : identifier = identifier;



sequence choice optional list

sequence(choice, choice, optional, list)
...

choice(choice, choice, optional, list)
