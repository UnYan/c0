package Tokenizer;

import error.TokenizeError;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;



public class TokenizerTest {
    private Tokenizer init(){
        File file = new File("/Users/lyx/bydzy/c0-compiler/Analysetest.txt");
        Scanner sc = null;
        try {
            sc = new Scanner(file);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        StringIter it = new StringIter(sc);
        Tokenizer tokenizer = new Tokenizer(it);
        return tokenizer;
    }



}
