/**
 * Copyright 2021 SPeCS.
 * <p>
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * <p>
 * http://www.apache.org/licenses/LICENSE-2.0
 * <p>
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License. under the License.
 */

package pt.up.fe.comp.jmm.ollir;

import org.specs.comp.ollir.ClassUnit;
import org.specs.comp.ollir.parser.OllirParser;

import pt.up.fe.comp.jmm.analysis.table.Symbol;
import pt.up.fe.comp.jmm.analysis.table.Type;

import pt.up.fe.specs.util.SpecsIo;

public class OllirUtils {

    /**
     * Parses OLLIR code.
     *
     * @param code
     * @return a ClassUnit representing all the information from the OLLIR code
     */
    public static ClassUnit parse(String code) {
        OllirParser parser = new OllirParser(SpecsIo.toInputStream(code));

        try {
            // parse the input OLLIR and represent it by the class structure used
            parser.ClassUnit();

            // get Class instance representing the OLLIR file loaded
            var classUnit = parser.getMyClass();

            // Automatically build var tables
            classUnit.buildVarTables();

            return classUnit;
            // TODO: Falta análise semântica no ollir, como forçar literais booleans a true/false,
            // toda semântica que lhes pedimos mais verificações de tipos e imports nas declaracoes
        } catch (Exception e) {
            // // Get cause
            // // Throwable currentException = e;
            // String causeMessage = e.getMessage();
            // if (e.getCause() != null) {
            // causeMessage = e.getCause().getMessage();
            // }
            // // while (currentException != null) {
            // // causeMessage = currentException.getMessage();
            // // currentException = currentException.getCause();
            // // }
            //
            // throw new RuntimeException(
            // "Error on line " + e.currentToken.beginLine + ": " + causeMessage, e);

            throw new RuntimeException("Exception while parsing OLLIR code:\n" + code, e);
        }

    }

    /*
     * Utility methods for translating OLLIR types
     */

    public static String getCode(Symbol symbol) {
        return symbol.getName() + "." + getCode(symbol.getType());
    }

    public static String getCode(Type type) {
        StringBuilder code = new StringBuilder();

        if (type.isArray()) {
            code.append("array.");
        }

        code.append(getOllirType(type.getName()));

        return code.toString();
    }

    public static String getOllirType(String jmmType) {
        switch (jmmType) {
            case "boolean":
                return "bool";
            case "int":
                return "i32";
            case "void":
                return "V";
            default:
                return jmmType;
        }
    }
}
