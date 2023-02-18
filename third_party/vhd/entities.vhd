LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;

PACKAGE click_element_library_constants IS
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
USE work.click_element_library_constants.ALL;

ENTITY decoupled_hs_reg IS
  GENERIC (
    DATA_WIDTH : NATURAL := 8;
    VALUE : NATURAL := 0;
    PHASE_INIT_IN : STD_LOGIC := '0';
    PHASE_INIT_OUT : STD_LOGIC := '0');
  PORT (
    rst : IN STD_LOGIC;
    -- Input channel
    in_ack : OUT STD_LOGIC;
    in_req : IN STD_LOGIC;
    in_data : IN STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
    -- Output channel
    out_req : OUT STD_LOGIC;
    out_data : OUT STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
    out_ack : IN STD_LOGIC);
END decoupled_hs_reg;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE work.click_element_library_constants.ALL;
ENTITY merge IS
  GENERIC (
    DATA_WIDTH : NATURAL := 8;
    PHASE_INIT_C : STD_LOGIC := '0';
    PHASE_INIT_A : STD_LOGIC := '0';
    PHASE_INIT_B : STD_LOGIC := '0');
  PORT (
    rst : IN STD_LOGIC;
    --Input channel 1
    inA_req : IN STD_LOGIC;
    inA_ack : OUT STD_LOGIC;
    inA_data : IN STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
    -- Input channel 2
    inB_req : IN STD_LOGIC;
    inB_ack : OUT STD_LOGIC;
    inB_data : IN STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
    -- Output channel
    outC_req : OUT STD_LOGIC;
    outC_data : OUT STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
    outC_ack : IN STD_LOGIC
  );
END merge;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE work.click_element_library_constants.ALL;
ENTITY mux IS
  --generic for initializing the phase registers
  GENERIC (
    DATA_WIDTH : NATURAL := 8;
    PHASE_INIT_C : STD_LOGIC := '0';
    PHASE_INIT_A : STD_LOGIC := '0';
    PHASE_INIT_B : STD_LOGIC := '0';
    PHASE_INIT_SEL : STD_LOGIC := '0');
  PORT (
    rst : IN STD_LOGIC; -- rst line
    -- Input from channel 1
    inA_req : IN STD_LOGIC;
    inA_data : IN STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
    inA_ack : OUT STD_LOGIC;
    -- Input from channel 2
    inB_req : IN STD_LOGIC;
    inB_data : IN STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
    inB_ack : OUT STD_LOGIC;
    -- Output port 
    outC_req : OUT STD_LOGIC;
    outC_data : OUT STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
    outC_ack : IN STD_LOGIC;
    -- Select port
    inSel_req : IN STD_LOGIC;
    inSel_ack : OUT STD_LOGIC;
    selector : IN STD_LOGIC_VECTOR(0 DOWNTO 0)
  );
END mux;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE work.click_element_library_constants.ALL;
ENTITY demux IS
  GENERIC (
    DATA_WIDTH : NATURAL := 8;
    PHASE_INIT_A : STD_LOGIC := '0';
    PHASE_INIT_B : STD_LOGIC := '0';
    PHASE_INIT_C : STD_LOGIC := '0'
  );
  PORT (
    rst : IN STD_LOGIC;
    -- Input port
    inA_req : IN STD_LOGIC;
    inA_data : IN STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
    inA_ack : OUT STD_LOGIC;
    -- Select port 
    inSel_req : IN STD_LOGIC;
    inSel_ack : OUT STD_LOGIC;
    selector : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    -- Output channel 1
    outB_req : OUT STD_LOGIC;
    outB_data : OUT STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
    outB_ack : IN STD_LOGIC;
    -- Output channel 2
    outC_req : OUT STD_LOGIC;
    outC_data : OUT STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
    outC_ack : IN STD_LOGIC
  );
END demux;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

USE ieee.std_logic_unsigned.ALL;
USE ieee.numeric_std.ALL;
USE work.click_element_library_constants.ALL;
ENTITY reg_fork IS
  GENERIC (
    DATA_WIDTH : NATURAL := 8;
    VALUE : NATURAL := 0;
    PHASE_INIT_A : STD_LOGIC := '0';
    PHASE_INIT_B : STD_LOGIC := '0';
    PHASE_INIT_C : STD_LOGIC := '0');
  PORT (
    rst : IN STD_LOGIC;
    --Input channel
    inA_req : IN STD_LOGIC;
    inA_data : IN STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
    inA_ack : OUT STD_LOGIC;
    --Output channel 1
    outB_req : OUT STD_LOGIC;
    outB_data : OUT STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
    outB_ack : IN STD_LOGIC;
    --Output channel 2
    outC_req : OUT STD_LOGIC;
    outC_data : OUT STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
    outC_ack : IN STD_LOGIC
  );
END reg_fork;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE work.click_element_library_constants.ALL;
ENTITY fork IS
  GENERIC (
    DATA_WIDTH : NATURAL := 8;
    PHASE_INIT : STD_LOGIC := '0');
  PORT (
    rst : IN STD_LOGIC;
    --Input channel
    inA_req : IN STD_LOGIC;
    inA_data : IN STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
    inA_ack : OUT STD_LOGIC;
    --Output channel 1
    outB_req : OUT STD_LOGIC;
    outB_data : OUT STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
    outB_ack : IN STD_LOGIC;
    --Output channel 2
    outC_req : OUT STD_LOGIC;
    outC_data : OUT STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
    outC_ack : IN STD_LOGIC
  );
END fork;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE work.click_element_library_constants.ALL;
use ieee.std_logic_misc.all;

ENTITY MultiHsJoin IS
  GENERIC (
    HANDSHAKE_COMPONENTS : NATURAL := 2;
    PHASE_INIT : STD_LOGIC := '0'
  );
  PORT (
    rst : IN STD_LOGIC;
    --Input handshakes
    in_req : IN STD_LOGIC_VECTOR(HANDSHAKE_COMPONENTS - 1 DOWNTO 0);
    in_ack : OUT STD_LOGIC_VECTOR(HANDSHAKE_COMPONENTS - 1 DOWNTO 0);

    --Output handshakes
    out_req : OUT STD_LOGIC;
    out_ack : IN STD_LOGIC
  );
END MultiHsJoin;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE work.click_element_library_constants.ALL;
use ieee.std_logic_misc.all;

ENTITY MultiHsFork IS
  GENERIC (
    HANDSHAKE_COMPONENTS : NATURAL := 2;
    PHASE_INIT : STD_LOGIC := '0'
  );
  PORT (
    rst : IN STD_LOGIC;
    --Input handshakes
    in_req : IN STD_LOGIC;
    in_ack : OUT STD_LOGIC;

    --Output handshakes
    out_req : OUT STD_LOGIC_VECTOR(HANDSHAKE_COMPONENTS - 1 DOWNTO 0);
    out_ack : IN STD_LOGIC_VECTOR(HANDSHAKE_COMPONENTS - 1 DOWNTO 0)
  );
END MultiHsFork;

ARCHITECTURE beh OF mux IS
  -- the registers
  SIGNAL phase_c, phase_sel, inSel_token : STD_LOGIC;
  -- register control
  SIGNAL phase_a : STD_LOGIC;
  SIGNAL phase_b : STD_LOGIC;
  -- Clock
  SIGNAL click_req, click_ack : STD_LOGIC;
  SIGNAL pulse : STD_LOGIC;
  -- control gates
  SIGNAL inA_token, inB_token : STD_LOGIC;
  SIGNAL selected_a, selected_b : STD_LOGIC;

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

  SIGNAL inA_token, inB_token, outC_bubble : STD_LOGIC;
  SIGNAL phase_a, phase_b, phase_c : STD_LOGIC;
  SIGNAL click : STD_LOGIC;
  SIGNAL data_reg, data_sig : STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);

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

  SIGNAL phase_a : STD_LOGIC;
  SIGNAL click_req, click_ack : STD_LOGIC;

  SIGNAL phase_b : STD_LOGIC;
  SIGNAL phase_c : STD_LOGIC;

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

ARCHITECTURE behavioural OF decoupled_hs_reg IS

  SIGNAL phase_in, phase_out, in_req_d, out_req_d : STD_LOGIC;
  SIGNAL data_sig : STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
  SIGNAL click : STD_LOGIC;

  ATTRIBUTE dont_touch : STRING;
  ATTRIBUTE dont_touch OF phase_in, phase_out : SIGNAL IS "true";
  ATTRIBUTE dont_touch OF data_sig : SIGNAL IS "true";
  ATTRIBUTE dont_touch OF click : SIGNAL IS "true";
  ATTRIBUTE dont_touch OF in_req_d, out_req_d : SIGNAL IS "true";

BEGIN
  out_req <= phase_out;
  in_ack <= phase_in;
  out_data <= data_sig;

  clock_regs : PROCESS (click, rst)
  BEGIN
    IF rst = '1' THEN
      phase_in <= PHASE_INIT_IN;
      phase_out <= PHASE_INIT_OUT;
      data_sig <= STD_LOGIC_VECTOR(to_unsigned(VALUE, DATA_WIDTH));
    ELSIF rising_edge(click) THEN
      phase_in <= NOT phase_in AFTER REG_CQ_DELAY;
      phase_out <= NOT phase_out AFTER REG_CQ_DELAY;
      data_sig <= in_data AFTER REG_CQ_DELAY;
    END IF;
  END PROCESS;

  delay_in_ack : ENTITY work.delay_element
    GENERIC MAP(
      NUM_LCELLS => 2 * LUT_CHAIN_SIZE -- Delay  size
    )
    PORT MAP(
      i => in_req,
      o => in_req_d
    );

  --delay_out_req : ENTITY work.delay_element
    --GENERIC MAP(
    --  NUM_LCELLS => 2 * LUT_CHAIN_SIZE -- Delay  size
    --)
    --PORT MAP(
    --  i => phase_out,
    --  o => out_req_d
   -- );

  click <= (in_req_d XOR phase_in) AND (out_ack XNOR phase_out) AFTER AND2_DELAY + XOR_DELAY;

END behavioural;

ARCHITECTURE beh OF fork IS

  SIGNAL click : STD_LOGIC;
  SIGNAL phase : STD_LOGIC := PHASE_INIT;

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

  SIGNAL click : STD_LOGIC;
  SIGNAL phase_a : STD_LOGIC;
  SIGNAL phase_b, phase_c, outB_bubble, outC_bubble, inA_token : STD_LOGIC;
  SIGNAL data_reg : STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);

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
      data_reg <= STD_LOGIC_VECTOR(to_unsigned(VALUE, DATA_WIDTH));
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

ARCHITECTURE beh OF multiHsFork IS

  SIGNAL click : STD_LOGIC;
  SIGNAL phase : STD_LOGIC := PHASE_INIT;

  ATTRIBUTE dont_touch : STRING;
  ATTRIBUTE dont_touch OF phase : SIGNAL IS "true";
  ATTRIBUTE dont_touch OF click : SIGNAL IS "true";
BEGIN
  -- Control Path
  out_req <= (others => in_req);
  in_ack <= phase;
  
  click <= (and_reduce(out_ack) AND NOT(phase)) OR (and_reduce(NOT(out_ack)) AND phase) AFTER AND3_DELAY + OR2_DELAY;
  
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

end beh;

ARCHITECTURE beh OF multiHsJoin IS

  SIGNAL click : STD_LOGIC;
  SIGNAL phase : STD_LOGIC := PHASE_INIT;

  ATTRIBUTE dont_touch : STRING;
  ATTRIBUTE dont_touch OF phase : SIGNAL IS "true";
  ATTRIBUTE dont_touch OF click : SIGNAL IS "true";

  SIGNAL in_req_and : STD_LOGIC := '0';
  SIGNAL in_req_nand : STD_LOGIC := '0';
BEGIN
  -- Control Path
  out_req <= phase;
  in_ack <= (others => out_ack);

  click <= (and_reduce(in_req) AND NOT(phase)) OR (and_reduce(NOT(in_req)) AND phase) AFTER AND3_DELAY + OR2_DELAY;
  
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

end beh;