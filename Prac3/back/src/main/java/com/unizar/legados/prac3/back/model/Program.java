package com.unizar.legados.prac3.back.model;


public class Program implements Comparable<Program> {
    private int register = -1;
    private String name;
    private String type;
    private String tape;

    public Program() {
    }

    public Program(String register, String name, String type, String tape) {
        try {
            this.register = Integer.parseInt(register);
        } catch (final NumberFormatException e) {
            this.register = -1;
        }
        this.name = name;
        this.type = type;
        this.tape = correctType(tape);
    }

    private String correctType(String type) {
        return type.replace("II", "IV");
    }

    public boolean isEmpty() {
        return register == -1 || tape == null || tape.isEmpty();
    }

    public boolean isNumeric(String tape) {
        return tape.matches("[0-9]+");
    }

    public boolean noMoreInTape(String tape) {
        return tape.length() > 0 && this.tape.length() > 0 && tape.charAt(0) < this.tape.charAt(0);
    }

    public boolean inTape(String tape) {
        boolean in = this.tape.equals(tape);
        if (!in) {
            if (this.tape.contains("-")) {
                String[] twoTape = this.tape.split("-");
                in = twoTape[1].equals(tape) || twoTape[0].equals(tape);
            } else if (this.tape.matches("V[0-9]+")) {
                in = tape.equals("V") || this.tape.substring(1, this.tape.length() - 1).equals(tape);
            }
        }
        return in;
    }


    @Override
    public String toString() {
        return "{" +
                " register:" + register +
                ", name:'" + name + '\'' +
                ", type:'" + type + '\'' +
                ", tape:'" + tape + '\'' +
                '}';
    }

    @Override
    public int compareTo(Program o) {
        return register - o.register;
    }
}
