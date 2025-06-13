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

public class FibTest {
    private final static String BASE_DIR = "pp/iloc/sample/";

    @Test
    public void test(){
        Machine c = new Machine();

        c.setNum("n", 15);
        new Simulator(parse("fib2"), c).run();
        int ret = c.getReg("r_z");

        assertEquals(610,ret);
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
