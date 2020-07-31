LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;

PACKAGE g2a_constants IS
  --Delay size
  CONSTANT ADD_DELAY : INTEGER := 15;
  CONSTANT LUT_CHAIN_SIZE : INTEGER := 10;
  CONSTANT AND2_DELAY : TIME := 2 ns; -- 2 input AND gate
  CONSTANT AND3_DELAY : TIME := 3 ns; -- 3 input AND gate
  CONSTANT NOT1_DELAY : TIME := 1 ns; -- 1 input NOT gate
  CONSTANT ANDOR3_DELAY : TIME := 4 ns; -- Complex AND_OR gate
  CONSTANT REG_CQ_DELAY : TIME := 1 ns; -- Clk to Q delay
  CONSTANT ADDER_DELAY : TIME := 15 ns; -- Adder delay

  CONSTANT OR2_DELAY : TIME := 2 ns; -- 2 input OR gate
  CONSTANT XOR_DELAY : TIME := 3 ns; --2 input XOR gate

END PACKAGE;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.g2a_constants.ALL;
ENTITY decoupled_hs_reg IS
  GENERIC (
    VARIABLE_WIDTH : NATURAL := 4;
    DATA_MULTIPLIER : NATURAL := 3;
    IN_DATA_WIDTH : NATURAL := 8;
    OUT_DATA_WIDTH : NATURAL := 8;
    VALUE : NATURAL := 0;
    PHASE_INIT_IN : std_logic := '0';
    PHASE_INIT_OUT : std_logic := '0');
  PORT (
    rst : IN std_logic;
    -- Input channel
    in_ack : OUT std_logic;
    in_req : IN std_logic;
    in_data : IN std_logic_vector(IN_DATA_WIDTH - 1 DOWNTO 0);
    -- Output channel
    out_req : OUT std_logic;
    out_data : OUT std_logic_vector(OUT_DATA_WIDTH - 1 DOWNTO 0);
    out_ack : IN std_logic);
END decoupled_hs_reg;
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.numeric_std.ALL;
USE work.g2a_constants.ALL;

ENTITY LoopBlock IS
  GENERIC (
    VARIABLE_WIDTH : NATURAL := 4;
    DATA_MULTIPLIER : NATURAL := 3;
    DATA_WIDTH : NATURAL := 8
  );
  PORT (
    rst : IN std_logic;
    -- Input channel
    in_req : IN std_logic;
    in_ack : OUT std_logic;
    in_data : IN std_logic_vector(DATA_WIDTH - 1 DOWNTO 0);
    -- Output channel
    out_req : OUT std_logic;
    out_ack : IN std_logic;
    out_data : OUT std_logic_vector(DATA_WIDTH - 1 DOWNTO 0));
END LoopBlock;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE work.g2a_constants.ALL;
ENTITY merge IS
  GENERIC (
    VARIABLE_WIDTH : NATURAL := 4;
    DATA_MULTIPLIER : NATURAL := 3;
    DATA_WIDTH : NATURAL := 8;
    PHASE_INIT_C : std_logic := '0';
    PHASE_INIT_A : std_logic := '0';
    PHASE_INIT_B : std_logic := '0');
  PORT (
    rst : IN std_logic;
    --Input channel 1
    inA_req : IN std_logic;
    inA_ack : OUT std_logic;
    inA_data : IN std_logic_vector(DATA_WIDTH - 1 DOWNTO 0);
    -- Input channel 2
    inB_req : IN std_logic;
    inB_ack : OUT std_logic;
    inB_data : IN std_logic_vector(DATA_WIDTH - 1 DOWNTO 0);
    -- Output channel
    outC_req : OUT std_logic;
    outC_data : OUT std_logic_vector(DATA_WIDTH - 1 DOWNTO 0);
    outC_ack : IN std_logic
  );
END merge;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.numeric_std.ALL;
USE work.g2a_constants.ALL;

ENTITY funcBlock IS
  GENERIC (
    VARIABLE_WIDTH : NATURAL := 4;
    DATA_MULTIPLIER : NATURAL := 3;
    DATA_WIDTH : NATURAL := 8
  );
  PORT (-- Input channel
    in_req : IN std_logic;
    in_ack : OUT std_logic;
    in_data : IN std_logic_vector(DATA_WIDTH - 1 DOWNTO 0);
    -- Output channel
    out_req : OUT std_logic;
    out_ack : IN std_logic;
    out_data : OUT std_logic_vector(DATA_WIDTH - 1 DOWNTO 0));
END funcBlock;
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE work.g2a_constants.ALL;
ENTITY mux IS
  --generic for initializing the phase registers
  GENERIC (
    VARIABLE_WIDTH : NATURAL := 4;
    DATA_MULTIPLIER : NATURAL := 3;
    DATA_WIDTH : NATURAL := 8;
    PHASE_INIT_C : std_logic := '0';
    PHASE_INIT_A : std_logic := '0';
    PHASE_INIT_B : std_logic := '0';
    PHASE_INIT_SEL : std_logic := '0');
  PORT (
    rst : IN std_logic; -- rst line
    -- Input from channel 1
    inA_req : IN std_logic;
    inA_data : IN std_logic_vector(DATA_WIDTH - 1 DOWNTO 0);
    inA_ack : OUT std_logic;
    -- Input from channel 2
    inB_req : IN std_logic;
    inB_data : IN std_logic_vector(DATA_WIDTH - 1 DOWNTO 0);
    inB_ack : OUT std_logic;
    -- Output port 
    outC_req : OUT std_logic;
    outC_data : OUT std_logic_vector(DATA_WIDTH - 1 DOWNTO 0);
    outC_ack : IN std_logic;
    -- Select port
    inSel_req : IN std_logic;
    inSel_ack : OUT std_logic;
    selector : IN std_logic_vector(0 DOWNTO 0)
  );
END mux;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE work.g2a_constants.ALL;
ENTITY demux IS
  GENERIC (
    VARIABLE_WIDTH : NATURAL := 4;
    DATA_MULTIPLIER : NATURAL := 3;
    DATA_WIDTH : NATURAL := 8;
    PHASE_INIT_A : std_logic := '0';
    PHASE_INIT_B : std_logic := '0';
    PHASE_INIT_C : std_logic := '0'
  );
  PORT (
    rst : IN std_logic;
    -- Input port
    inA_req : IN std_logic;
    inA_data : IN std_logic_vector(DATA_WIDTH - 1 DOWNTO 0);
    inA_ack : OUT std_logic;
    -- Select port 
    inSel_req : IN std_logic;
    inSel_ack : OUT std_logic;
    selector : IN std_logic_vector(0 DOWNTO 0);
    -- Output channel 1
    outB_req : OUT std_logic;
    outB_data : OUT std_logic_vector(DATA_WIDTH - 1 DOWNTO 0);
    outB_ack : IN std_logic;
    -- Output channel 2
    outC_req : OUT std_logic;
    outC_data : OUT std_logic_vector(DATA_WIDTH - 1 DOWNTO 0);
    outC_ack : IN std_logic
  );
END demux;
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

USE ieee.std_logic_unsigned.ALL;
USE ieee.numeric_std.ALL;
USE work.g2a_constants.ALL;
ENTITY reg_fork IS
  GENERIC (
    VARIABLE_WIDTH : NATURAL := 4;
    DATA_MULTIPLIER : NATURAL := 3;
    DATA_WIDTH : NATURAL := 8;
    VALUE : NATURAL := 0;
    PHASE_INIT_A : std_logic := '0';
    PHASE_INIT_B : std_logic := '0';
    PHASE_INIT_C : std_logic := '0');
  PORT (
    rst : IN std_logic;
    --Input channel
    inA_req : IN std_logic;
    inA_data : IN std_logic_vector(DATA_WIDTH - 1 DOWNTO 0);
    inA_ack : OUT std_logic;
    --Output channel 1
    outB_req : OUT std_logic;
    outB_data : OUT std_logic_vector(DATA_WIDTH - 1 DOWNTO 0);
    outB_ack : IN std_logic;
    --Output channel 2
    outC_req : OUT std_logic;
    outC_data : OUT std_logic_vector(DATA_WIDTH - 1 DOWNTO 0);
    outC_ack : IN std_logic
    );
END reg_fork;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE work.g2a_constants.ALL;
ENTITY fork IS
  GENERIC (
    VARIABLE_WIDTH : NATURAL := 4;
    DATA_MULTIPLIER : NATURAL := 3;
    DATA_WIDTH : NATURAL := 8;
    PHASE_INIT : std_logic := '0');
  PORT (
    rst : IN std_logic;
    --Input channel
    inA_req : IN std_logic;
    inA_data : IN std_logic_vector(DATA_WIDTH - 1 DOWNTO 0);
    inA_ack : OUT std_logic;
    --Output channel 1
    outB_req : OUT std_logic;
    outB_data : OUT std_logic_vector(DATA_WIDTH - 1 DOWNTO 0);
    outB_ack : IN std_logic;
    --Output channel 2
    outC_req : OUT std_logic;
    outC_data : OUT std_logic_vector(DATA_WIDTH - 1 DOWNTO 0);
    outC_ack : IN std_logic
  );
END fork;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.numeric_std.ALL;
USE work.g2a_constants.ALL;

ENTITY Selector IS
  GENERIC (
    VARIABLE_WIDTH : NATURAL := 4;
    DATA_MULTIPLIER : NATURAL := 3;
    DATA_WIDTH : NATURAL := 8
  );
  PORT (
    -- Data
    in_data : IN std_logic_vector(DATA_WIDTH - 1 DOWNTO 0);
    in_req : IN std_logic;
    in_ack : OUT std_logic;
    -- Selector
    selector : OUT std_logic_vector(0 DOWNTO 0);
    out_req : OUT std_logic;
    out_ack : IN std_logic
  );
END Selector;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

LIBRARY altera_mf;
USE altera_mf.altera_mf_components.ALL;

ENTITY delay_element IS
  GENERIC (
    NUM_LCELLS : INTEGER := 5
  );
  PORT (
    i : IN std_logic;
    o : OUT std_logic
  );
END ENTITY;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.numeric_std.ALL;
USE work.g2a_constants.ALL;

ENTITY IfBlock IS
  GENERIC (
    VARIABLE_WIDTH : NATURAL := 4;
    DATA_MULTIPLIER : NATURAL := 3;
    DATA_WIDTH : NATURAL := 8
  );
  PORT (
    rst : IN std_logic;-- Input channel
    in_req : IN std_logic;
    in_ack : OUT std_logic;
    in_data : IN std_logic_vector(DATA_WIDTH - 1 DOWNTO 0);
    -- Output channel
    out_req : OUT std_logic;
    out_ack : IN std_logic;
    out_data : OUT std_logic_vector(DATA_WIDTH - 1 DOWNTO 0));
END IfBlock;
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

USE ieee.std_logic_unsigned.ALL;
USE ieee.numeric_std.ALL;
USE work.g2a_constants.ALL;

ENTITY BlockC IS
  GENERIC (
    VARIABLE_WIDTH : NATURAL := 4;
    DATA_MULTIPLIER : NATURAL := 3;
    DATA_WIDTH : NATURAL := 8
  );
  PORT (
    rst : IN std_logic;
    -- Input channel
    -- Data
    in_data : IN std_logic_vector(DATA_WIDTH - 1 DOWNTO 0);
    in_req : IN std_logic;
    in_ack : OUT std_logic;
    -- Selector
    out_data : OUT std_logic_vector(DATA_WIDTH - 1 DOWNTO 0);
    out_req : OUT std_logic;
    out_ack : IN std_logic
  );
END BlockC;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

USE ieee.std_logic_unsigned.ALL;
USE ieee.numeric_std.ALL;
USE work.g2a_constants.ALL;

ENTITY Scope IS
  GENERIC (
    VARIABLE_WIDTH : NATURAL := 4;
    DATA_MULTIPLIER : NATURAL := 3;
    DATA_WIDTH : NATURAL := 8;
    IN_DATA_WIDTH : NATURAL := 8;
    OUT_DATA_WIDTH : NATURAL := 8
  );
  PORT (
    rst : IN std_logic;
    -- Input channel
    -- Data
    in_data : IN std_logic_vector(IN_DATA_WIDTH - 1 DOWNTO 0);
    in_req : IN std_logic;
    in_ack : OUT std_logic;
    -- Selector
    out_data : OUT std_logic_vector(OUT_DATA_WIDTH - 1 DOWNTO 0);
    out_req : OUT std_logic;
    out_ack : IN std_logic
  );
END Scope;
ARCHITECTURE beh OF mux IS
  -- the registers
  SIGNAL phase_c, phase_sel, inSel_token : std_logic;
  -- register control
  SIGNAL phase_a : std_logic;
  SIGNAL phase_b : std_logic;
  -- Clock
  SIGNAL click_req, click_ack : std_logic;
  SIGNAL pulse : std_logic;
  -- control gates
  SIGNAL inA_token, inB_token : std_logic;
  SIGNAL selected_a, selected_b : std_logic;

  ATTRIBUTE dont_touch : STRING;
  ATTRIBUTE dont_touch OF phase_sel, phase_c, phase_a, phase_b : SIGNAL IS "true";
  ATTRIBUTE dont_touch OF click_req, click_ack : SIGNAL IS "true";

BEGIN
  -- Control Path
  inSel_ack <= phase_sel;
  outC_req <= phase_c;
  outC_data <= inA_data WHEN selector(0) = '1' ELSE
    inB_data;
  inA_ack <= phase_a;
  inB_ack <= phase_b;

  --input state
  inA_token <= phase_a XOR inA_req AFTER XOR_DELAY;
  inB_token <= phase_b XOR inB_req AFTER XOR_DELAY;
  inSel_token <= phase_sel XOR inSel_req AFTER XOR_DELAY;

  --Selector triggered pulse
  click_req <= (inA_token AND inSel_token AND selector(0)) OR (inB_token AND inSel_token AND NOT selector(0)) AFTER AND2_DELAY + OR2_DELAY;

  --Output state
  click_ack <= phase_c XNOR outC_ack AFTER XOR_DELAY;

  req_regs : PROCESS (click_req, rst)
  BEGIN
    IF rst = '1' THEN
      phase_c <= PHASE_INIT_C;
    ELSIF rising_edge(click_req) THEN
      -- Click control register loops back to itself
      phase_c <= NOT phase_c AFTER REG_CQ_DELAY;
    END IF;
  END PROCESS;

  ack_regs : PROCESS (click_ack, rst)
  BEGIN
    IF rst = '1' THEN
      phase_a <= PHASE_INIT_A;
      phase_b <= PHASE_INIT_B;
      phase_sel <= PHASE_INIT_SEL;
    ELSIF rising_edge(click_ack) THEN
      phase_a <= phase_a XOR selector(0) AFTER REG_CQ_DELAY;
      phase_b <= phase_b XOR NOT(selector(0)) AFTER REG_CQ_DELAY;
      phase_sel <= inSel_req AFTER REG_CQ_DELAY;
    END IF;
  END PROCESS;

END beh;

ARCHITECTURE behavioural OF merge IS

  SIGNAL inA_token, inB_token, outC_bubble : std_logic;
  SIGNAL phase_a, phase_b, phase_c : std_logic;
  SIGNAL click : std_logic;
  SIGNAL data_reg, data_sig : std_logic_vector(DATA_WIDTH - 1 DOWNTO 0);

  ATTRIBUTE dont_touch : STRING;
  ATTRIBUTE dont_touch OF phase_c, phase_a, phase_b, inA_token, inB_token : SIGNAL IS "true";
  ATTRIBUTE dont_touch OF click : SIGNAL IS "true";

BEGIN
  inA_token <= inA_req XOR phase_a AFTER XOR_DELAY;
  inB_token <= inB_req XOR phase_b AFTER XOR_DELAY;
  outC_bubble <= phase_c XNOR outC_ack AFTER XOR_DELAY;
  -- Click function
  click <= inA_token OR inB_token AFTER OR2_DELAY;

  clock_req : PROCESS (click, rst)
  BEGIN
    IF rst = '1' THEN
      phase_c <= PHASE_INIT_C;
    ELSIF rising_edge(click) THEN
      phase_c <= NOT phase_c AFTER REG_CQ_DELAY;
    END IF;
  END PROCESS;
  clock_ack : PROCESS (outC_bubble, rst)
  BEGIN
    IF rst = '1' THEN
      phase_a <= PHASE_INIT_A;
      phase_b <= PHASE_INIT_B;
    ELSIF rising_edge(outC_bubble) THEN
      phase_a <= inA_req AFTER REG_CQ_DELAY;
      phase_b <= inB_req AFTER REG_CQ_DELAY;
    END IF;
  END PROCESS;

  outC_data <= inA_data WHEN inA_token = '1' ELSE
    inB_data WHEN inB_token = '1' ELSE
    (OTHERS => '0');
  outC_req <= phase_c;
  inA_ack <= phase_a;
  inB_ack <= phase_b;

END behavioural;

ARCHITECTURE beh OF demux IS

  SIGNAL phase_a : std_logic;
  SIGNAL click_req, click_ack : std_logic;

  SIGNAL phase_b : std_logic;
  SIGNAL phase_c : std_logic;

BEGIN

  -- Control Path   
  inSel_ack <= phase_a;
  inA_ack <= phase_a;
  outB_req <= phase_b;
  outB_data <= inA_data;
  outC_req <= phase_c;
  outC_data <= inA_data;

  -- Request FF clock function
  click_req <= (inSel_req AND NOT(phase_a) AND inA_req) OR (NOT(inSel_req) AND phase_a AND NOT(inA_req)) AFTER ANDOR3_DELAY + NOT1_DELAY;

  -- Acknowledge FF clock function
  click_ack <= (outB_ack XNOR phase_b) AND (outC_ack XNOR phase_c) AFTER AND2_DELAY + XOR_DELAY + NOT1_DELAY;

  req : PROCESS (click_req, rst)
  BEGIN
    IF rst = '1' THEN
      phase_b <= PHASE_INIT_B;
      phase_c <= PHASE_INIT_C;
    ELSIF rising_edge(click_req) THEN
      phase_b <= phase_b XOR selector(0) AFTER REG_CQ_DELAY;
      phase_c <= phase_c XOR NOT(selector(0)) AFTER REG_CQ_DELAY;
    END IF;
  END PROCESS req;

  ack : PROCESS (click_ack, rst)
  BEGIN
    IF rst = '1' THEN
      phase_a <= PHASE_INIT_A;
    ELSIF rising_edge(click_ack) THEN
      phase_a <= NOT phase_a AFTER REG_CQ_DELAY;
    END IF;
  END PROCESS ack;

END beh;
ARCHITECTURE arch OF delay_element IS
  SIGNAL s : std_logic_vector(NUM_LCELLS DOWNTO 0);
  ATTRIBUTE preserve : BOOLEAN;
  ATTRIBUTE preserve OF s : SIGNAL IS true;
BEGIN
  s(0) <= i;
  o <= s(NUM_LCELLS);
  g_luts : FOR i IN 0 TO NUM_LCELLS - 1 GENERATE
    cmp_LUT : LCELL
    PORT MAP(
      a_in => s(i),
      a_out => s(i + 1)
    );
  END GENERATE;

END ARCHITECTURE;

ARCHITECTURE behavioural OF decoupled_hs_reg IS

  SIGNAL phase_in, phase_out : std_logic;
  SIGNAL data_sig : std_logic_vector(OUT_DATA_WIDTH - 1 DOWNTO 0);
  SIGNAL click : std_logic;

  ATTRIBUTE dont_touch : STRING;
  ATTRIBUTE dont_touch OF phase_in, phase_out : SIGNAL IS "true";
  ATTRIBUTE dont_touch OF data_sig : SIGNAL IS "true";
  ATTRIBUTE dont_touch OF click : SIGNAL IS "true";

BEGIN
  out_req <= phase_out;
  in_ack <= phase_in;
  out_data <= data_sig;

  clock_regs : PROCESS (click, rst)
  BEGIN
    IF rst = '1' THEN
      phase_in <= PHASE_INIT_IN;
      phase_out <= PHASE_INIT_OUT;
      data_sig <= std_logic_vector(to_unsigned(VALUE, OUT_DATA_WIDTH));
    ELSIF rising_edge(click) THEN
      phase_in <= NOT phase_in AFTER REG_CQ_DELAY;
      phase_out <= NOT phase_out AFTER REG_CQ_DELAY;
      data_sig <= in_data AFTER REG_CQ_DELAY;
    END IF;
  END PROCESS;

  click <= (in_req XOR phase_in) AND (out_ack XNOR phase_out) AFTER AND2_DELAY + XOR_DELAY;

END behavioural;

ARCHITECTURE beh OF fork IS

  SIGNAL click : std_logic;
  SIGNAL phase : std_logic := PHASE_INIT;

  ATTRIBUTE dont_touch : STRING;
  ATTRIBUTE dont_touch OF phase : SIGNAL IS "true";
  ATTRIBUTE dont_touch OF click : SIGNAL IS "true";

BEGIN
  -- Control Path
  outB_req <= inA_req;
  outC_req <= inA_req;
  outB_data <= inA_data;
  outC_data <= inA_data;
  inA_ack <= phase;

  click <= (outC_ack AND outB_ack AND NOT(phase)) OR (NOT(outC_ack) AND NOT(outB_ack) AND phase) AFTER AND3_DELAY + OR2_DELAY;

  clock_regs : PROCESS (click, rst)
  BEGIN
    IF rst = '1' THEN
      phase <= PHASE_INIT;
    ELSE
      IF rising_edge(click) THEN
        phase <= NOT phase AFTER REG_CQ_DELAY;
      END IF;
    END IF;
  END PROCESS clock_regs;

END beh;


ARCHITECTURE beh OF reg_fork IS

  SIGNAL click : std_logic;
  SIGNAL phase_a : std_logic;
  SIGNAL phase_b, phase_c, outB_bubble, outC_bubble, inA_token : std_logic;
  SIGNAL data_reg : std_logic_vector(DATA_WIDTH - 1 DOWNTO 0);

  ATTRIBUTE dont_touch : STRING;
  ATTRIBUTE dont_touch OF phase_b, phase_a, phase_c : SIGNAL IS "true";
  ATTRIBUTE dont_touch OF data_reg : SIGNAL IS "true";
  ATTRIBUTE dont_touch OF click : SIGNAL IS "true";
BEGIN
  inA_token <= inA_req XOR phase_a AFTER XOR_DELAY;
  outB_bubble <= phase_b XNOR outB_ack AFTER XOR_DELAY + NOT1_DELAY;
  outC_bubble <= phase_c XNOR outC_ack AFTER XOR_DELAY + NOT1_DELAY;
  -------------------------------------------------------

  click <= inA_token AND outB_bubble AND outC_bubble AFTER AND3_DELAY;

  clock_regs : PROCESS (click, rst)
  BEGIN
    IF rst = '1' THEN
      phase_a <= PHASE_INIT_A;
      phase_b <= PHASE_INIT_B;
      phase_c <= PHASE_INIT_C;
      data_reg <= std_logic_vector(to_unsigned(VALUE, DATA_WIDTH));
    ELSIF rising_edge(click) THEN
      phase_a <= NOT phase_a AFTER REG_CQ_DELAY;
      phase_b <= NOT phase_b AFTER REG_CQ_DELAY;
      phase_c <= NOT phase_c AFTER REG_CQ_DELAY;
      data_reg <= inA_data AFTER REG_CQ_DELAY;
    END IF;
  END PROCESS clock_regs;

  inA_ack <= phase_a;
  outB_req <= phase_b;
  outC_req <= phase_c;
  outB_data <= data_reg;
  outC_data <= data_reg;

END beh;