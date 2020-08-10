LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

LIBRARY altera_mf;
USE altera_mf.altera_mf_components.ALL;

ENTITY delay_element IS
  GENERIC (
    NUM_LCELLS : INTEGER := 0
  );
  PORT (
    i : IN std_logic;
    o : OUT std_logic
  );
END ENTITY;


ARCHITECTURE arch OF delay_element IS
  SIGNAL s : std_logic_vector(NUM_LCELLS DOWNTO 0);
  ATTRIBUTE preserve : BOOLEAN;
  ATTRIBUTE preserve OF s : SIGNAL IS true;
BEGIN
  s(0) <= i;
  o <= s(NUM_LCELLS) after NUM_LCELLS * 1ns;
  g_luts : FOR i IN 0 TO NUM_LCELLS - 1 GENERATE
    cmp_LUT : LCELL
    PORT MAP(
      a_in => s(i),
      a_out => s(i + 1)
    );
  END GENERATE;

END ARCHITECTURE;