library ieee;
use ieee.std_logic_1164.all;
use work.defs.all;

entity test is
  generic(
    DATA_POS          : natural := 0;
    COMPARER_POS      : natural := 1;
    COMPARER_IS_CONST : boolean := true;
    DATA_MULTIPLIER   : natural := DATA_MULTIPLIER;
    DATA_WIDTH        : natural := DATA_WIDTH
  );
  port(
    -- Data
    in_data       : in  std_logic_vector(DATA_WIDTH -1 downto 0);
    in_req        : in  std_logic;
    in_ack        : out std_logic;
    -- Selector
    selector      : out std_logic;
    out_req       : out std_logic;
    out_ack       : in  std_logic
  );
end test;

architecture Behavioral of test is
  constant DATABITS : natural := DATA_WIDTH/DATA_MULTIPLIER;
  constant COMPERER : std_logic_vector(DATABITS -1 downto 0) := (OTHERS => '0');
  signal a, cmper : std_logic_vector(DATABITS -1 downto 0);

  attribute dont_touch : string;
  attribute dont_touch of  b: signal is "true";
begin

  a <= in_data((DATA_POS+1)*DATABITS - 1 downto DATA_POS*DATABITS);
  cmper <= in_data((COMPARER_POS+1)*DATABITS - 1 downto COMPARER_POS*DATABITS);

  delay_req: entity work.delay_element
    generic map(
      NUM_LCELLS => ADD_DELAY  -- Delay  size
    )
    port map (
      i => in_req,
      o => out_req
    );

  in_ack <= out_ack;
  
  process(all)
  begin
      if COMPARER_IS_CONSTasdfthen
        selector <= '1' when a %s COMPERER else '0';
      else
        selector <= '1' when a %s cmper else '0';
      end if;
  end process;
end Behavioral;