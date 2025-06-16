package pp.iloc.test;
import org.junit.jupiter.api.Test;
import pp.iloc.Assembler;
import pp.iloc.Simulator;
import pp.iloc.eval.Machine;
import pp.iloc.model.Program;
import pp.iloc.parse.FormatException;

import java.io.File;
import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

public class DisassemblerTest {


    private final static String BASE_DIR = "pp/iloc/sample/";
    private final static boolean SHOW = true;

    @Test
    public void testMax(){
        String file = "max";
        Program p = parse(file);

        String print = p.prettyPrint();
        if (SHOW) {
            System.out.println("Program " + file + ":");
            System.out.print(print);
        }

        try{
            Program other = Assembler.instance().assemble(print);
            assertEquals(p, other);
        } catch (FormatException e) {
            throw new RuntimeException(e);
        }
    }

    @Test
    public void TestRun(){
        Machine c = new Machine();

        int a = c.init("a", 1,2,3,4,6,7);
        c.setNum("alength", 6);
        new Simulator(parse("max"), c).run();
        int max = c.getReg("r_max");

        assertEquals(7, max);
    }

    Program parse(String filename) {
        File file = new File(filename + ".iloc");
        if (!file.exists()) {
            file = new File(BASE_DIR + filename + ".iloc");
        }
        try {
            Program result = Assembler.instance().assemble(file);
            return result;
        } catch (FormatException | IOException e) {
            fail(e.getMessage());
            return null;
        }
    }
}
