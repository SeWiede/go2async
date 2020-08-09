library IEEE;
	use IEEE.std_logic_1164.all;
	
	package defs is
	constant GCD_DATA_WIDTH : Integer := 16;
	constant GCD_OUT_DATA_WIDTH : Integer := 4;
	constant GCD_IN_DATA_WIDTH : Integer := 8;
	
end package;
architecture beh_cl_4 of funcBlock is
	alias x      : std_logic_vector(4 - 1 downto 0)  is in_data( 12 - 1 downto 8);
alias result : std_logic_vector(4 - 1 downto 0)  is out_data( 4 - 1 downto 0);

	  
    --attribute dont_touch : string;
	--attribute dont_touch of  x, y, result: signal is "true";
	  
    --attribute keep : boolean;
	--attribute keep of  x, y, result: signal is true;
  begin
    in_ack <= out_ack;
    
    delay_req: entity work.delay_element
      generic map(
        NUM_LCELLS => ADD_DELAY  -- Delay  size
      )
      port map (
        i => in_req,
        o => out_req
	  );
	  

	  calc: process(all)
	variable offset: integer range 0 to out_data'length;
	begin
	out_data <= in_data; 
	result <= std_logic_vector(resize(unsigned(x)  , result'length)) ;

	end process;
  end beh_cl_4;
  architecture beh_b_1 of BlockC is
	signal cl_0_o_req, cl_0_o_ack : std_logic;signal cl_0_data: std_logic_vector(DATA_WIDTH -1 downto 0);
signal cl_1_o_req, cl_1_o_ack : std_logic;signal cl_1_data: std_logic_vector(DATA_WIDTH -1 downto 0);
signal lb_0_o_req, lb_0_o_ack : std_logic;signal lb_0_data: std_logic_vector(DATA_WIDTH -1 downto 0);
signal cl_3_o_req, cl_3_o_ack : std_logic;signal cl_3_data: std_logic_vector(DATA_WIDTH -1 downto 0);
signal cl_4_o_req, cl_4_o_ack : std_logic;signal cl_4_data: std_logic_vector(DATA_WIDTH -1 downto 0);

begin
out_req <= cl_4_o_req; 
cl_4_o_ack <= out_ack; 
out_data <= cl_4_data; 

CL_0: entity work.funcBlock(beh_cl_0)
	generic map(
	  DATA_WIDTH => DATA_WIDTH
	)
	port map (
	  -- Input channel
	  in_req  => in_req,
	  in_ack  => in_ack, 
	  in_data => in_data,
	  -- Output channel
	  out_req => cl_0_o_req,
	  out_ack => cl_0_o_ack,
	  out_data  => cl_0_data
	);
	CL_1: entity work.funcBlock(beh_cl_1)
	generic map(
	  DATA_WIDTH => DATA_WIDTH
	)
	port map (
	  -- Input channel
	  in_req  => cl_0_o_req,
	  in_ack  => cl_0_o_ack, 
	  in_data => cl_0_data,
	  -- Output channel
	  out_req => cl_1_o_req,
	  out_ack => cl_1_o_ack,
	  out_data  => cl_1_data
	);
	LB_0: entity work.LoopBlock(beh_lb_0)
  generic map(
    DATA_WIDTH => DATA_WIDTH
  )
  port map (
    rst => rst,
    in_ack => cl_1_o_ack,
    in_req => cl_1_o_req,
    in_data => cl_1_data,
    -- Output channel
    out_req => lb_0_o_req,
    out_data => lb_0_data,
    out_ack => lb_0_o_ack
  );
  CL_3: entity work.funcBlock(beh_cl_3)
	generic map(
	  DATA_WIDTH => DATA_WIDTH
	)
	port map (
	  -- Input channel
	  in_req  => lb_0_o_req,
	  in_ack  => lb_0_o_ack, 
	  in_data => lb_0_data,
	  -- Output channel
	  out_req => cl_3_o_req,
	  out_ack => cl_3_o_ack,
	  out_data  => cl_3_data
	);
	CL_4: entity work.funcBlock(beh_cl_4)
	generic map(
	  DATA_WIDTH => DATA_WIDTH
	)
	port map (
	  -- Input channel
	  in_req  => cl_3_o_req,
	  in_ack  => cl_3_o_ack, 
	  in_data => cl_3_data,
	  -- Output channel
	  out_req => cl_4_o_req,
	  out_ack => cl_4_o_ack,
	  out_data  => cl_4_data
	);
	end beh_b_1;
	architecture beh_se_1 of Selector is
	alias x      : std_logic_vector(4 - 1 downto 0)  is in_data( 16 - 1 downto 12);
alias y      : std_logic_vector(4 - 1 downto 0)  is in_data( 8 - 1 downto 4);

  
    --attribute dont_touch : string;
	--attribute dont_touch of  x, y, selector: signal is "true";
	
	--attribute keep : boolean;
	--attribute keep of  x, y, selector: signal is true;
  begin
  
    delay_req: entity work.delay_element
      generic map(
        NUM_LCELLS => ADD_DELAY  -- Delay  size
      )
      port map (
        i => in_req,
        o => out_req
      );
  
    in_ack <= out_ack;
    
    selector(0) <= '0' when unsigned(x) >= unsigned(y) else '1';


  end beh_se_1;
  architecture beh_b_2 of BlockC is
	signal cl_2_o_req, cl_2_o_ack : std_logic;signal cl_2_data: std_logic_vector(DATA_WIDTH -1 downto 0);

begin
out_req <= cl_2_o_req; 
cl_2_o_ack <= out_ack; 
out_data <= cl_2_data; 

CL_2: entity work.funcBlock(beh_cl_2)
	generic map(
	  DATA_WIDTH => DATA_WIDTH
	)
	port map (
	  -- Input channel
	  in_req  => in_req,
	  in_ack  => in_ack, 
	  in_data => in_data,
	  -- Output channel
	  out_req => cl_2_o_req,
	  out_ack => cl_2_o_ack,
	  out_data  => cl_2_data
	);
	end beh_b_2;
	architecture beh_cl_2 of funcBlock is
	alias x      : std_logic_vector(4 - 1 downto 0)  is in_data( 16 - 1 downto 12);
alias y      : std_logic_vector(4 - 1 downto 0)  is in_data( 8 - 1 downto 4);
alias result : std_logic_vector(4 - 1 downto 0)  is out_data( 16 - 1 downto 12);

	  
    --attribute dont_touch : string;
	--attribute dont_touch of  x, y, result: signal is "true";
	  
    --attribute keep : boolean;
	--attribute keep of  x, y, result: signal is true;
  begin
    in_ack <= out_ack;
    
    delay_req: entity work.delay_element
      generic map(
        NUM_LCELLS => ADD_DELAY  -- Delay  size
      )
      port map (
        i => in_req,
        o => out_req
	  );
	  

	  calc: process(all)
	variable offset: integer range 0 to out_data'length;
	begin
	out_data <= in_data; 
	result <= std_logic_vector(resize(unsigned(x) - unsigned(y), result'length))  after ADDER_DELAY;

	end process;
  end beh_cl_2;
  architecture beh_lb_0 of LoopBlock is
	signal cl_2_o_req, cl_2_o_ack : std_logic;signal cl_2_data: std_logic_vector(DATA_WIDTH -1 downto 0);
signal mx_0_o_req, mx_0_o_ack : std_logic;signal mx_0_data: std_logic_vector(DATA_WIDTH -1 downto 0);
signal rf_0_b_o_req, rf_0_b_o_ack : std_logic;signal rf_0_b_data: std_logic_vector(DATA_WIDTH -1 downto 0);
signal rf_0_c_o_req, rf_0_c_o_ack : std_logic;signal rf_0_c_data: std_logic_vector(DATA_WIDTH -1 downto 0);
signal se_1_o_req, se_1_o_ack : std_logic;signal se_1_select: std_logic_vector(1-1 downto 0);
signal f_0_b_o_req, f_0_b_o_ack : std_logic;
signal f_0_c_o_req, f_0_c_o_ack : std_logic;
signal lb_0_o_req, lb_0_o_ack : std_logic;signal lb_0_data: std_logic_vector(DATA_WIDTH -1 downto 0);
signal dx_0_c_o_req, dx_0_c_o_ack : std_logic;signal dx_0_c_data: std_logic_vector(DATA_WIDTH -1 downto 0);
signal r_1_o_req, r_1_o_ack : std_logic;signal r_1_data: std_logic_vector(DATA_WIDTH -1 downto 0);
signal r_0_o_req, r_0_o_ack : std_logic;signal r_0_data : std_logic_vector(0 downto 0);

begin
out_req <= lb_0_o_req; 
lb_0_o_ack <= out_ack; 
out_data <= lb_0_data; 

MX_0: entity work.mux
  generic map (
    DATA_WIDTH => DATA_WIDTH
  )
  port map (
    inA_ack => in_ack,
    inA_data => in_data,
    inA_req => in_req,
    inB_ack => cl_2_o_ack,
    inB_data => cl_2_data,
    inB_req => cl_2_o_req,
    outC_ack => mx_0_o_ack,
    outC_data => mx_0_data,
    outC_req => mx_0_o_req,
    rst => rst,
    inSel_ack => r_0_o_ack,
    inSel_req => r_0_o_req,
    selector => r_0_data
  );
  
RF_0: entity work.reg_fork
  generic map(
    DATA_WIDTH => DATA_WIDTH,
    PHASE_INIT_A => '0',
    PHASE_INIT_B =>'0',
    PHASE_INIT_C => '0')
  port map (
    inA_ack => mx_0_o_ack,
    inA_data => mx_0_data,
    inA_req => mx_0_o_req,
    outB_ack => rf_0_b_o_ack,
    outB_data => rf_0_b_data,
    outB_req => rf_0_b_o_req,
    outC_ack => rf_0_c_o_ack,
    outC_data=> rf_0_c_data,
    outC_req => rf_0_c_o_req,
    rst => rst
  );
SE_1: entity work.Selector(beh_se_1)
	generic map(
	  DATA_WIDTH => DATA_WIDTH
	)
	port map (
	  -- Input channel
	  in_req  => rf_0_b_o_req,
	  in_ack  => rf_0_b_o_ack, 
	  in_data => rf_0_b_data,
	  -- Output channel
	  out_req => se_1_o_req,
	  out_ack => se_1_o_ack,
	  selector  => se_1_select
	);
F_0: entity work.fork
  generic map(
    DATA_WIDTH => 1,
    PHASE_INIT => '0'
  )
  port map(
    inA_ack => se_1_o_ack,
    inA_req => se_1_o_req,
    inA_data => se_1_select,
    -- Output Channel
    outB_ack => f_0_b_o_ack,
    outB_req => f_0_b_o_req,
    outB_data => open,
    outC_ack => f_0_c_o_ack,
    outC_req => f_0_c_o_req,
    outC_data => open,
    rst => rst
  );
    
R_0: entity work.decoupled_hs_reg(behavioural)
  generic map (
    DATA_WIDTH => 1,
    VALUE => 1,
    PHASE_INIT_IN => '0',
    PHASE_INIT_OUT => '1'
  )
  port map (
    rst => rst,
    in_ack => f_0_b_o_ack,
    in_req => f_0_b_o_req,
    in_data => se_1_select,
    -- Output channel
    out_req => r_0_o_req,
    out_data => r_0_data,
    out_ack => r_0_o_ack
  );
  
DX_0: entity work.demux
  generic map (
    DATA_WIDTH => DATA_WIDTH
  )
  port map (
    inA_ack => rf_0_c_o_ack,
    inA_data => rf_0_c_data,
    inA_req => rf_0_c_o_req,
    outB_ack => lb_0_o_ack,
    outB_data => lb_0_data,
    outB_req => lb_0_o_req,
    outC_ack => dx_0_c_o_ack,
    outC_data => dx_0_c_data,
    outC_req => dx_0_c_o_req,
    rst => rst,
    inSel_ack => f_0_c_o_ack,
    inSel_req => f_0_c_o_req,
    selector => se_1_select
  );
   
R_1: entity work.decoupled_hs_reg(behavioural)
  generic map (
    DATA_WIDTH => DATA_WIDTH,
    VALUE => 0,
    PHASE_INIT_IN => '0',
    PHASE_INIT_OUT => '0'
  )
  port map (
    rst => rst,
    in_ack => dx_0_c_o_ack,
    in_req => dx_0_c_o_req,
    in_data => dx_0_c_data,
    -- Output channel
    out_req => r_1_o_req,
    out_data => r_1_data,
    out_ack => r_1_o_ack
  );
  
B_2: entity work.BlockC(beh_b_2)
  generic map(
   DATA_WIDTH => DATA_WIDTH
  )
  port map (
   rst => rst,
   in_ack => r_1_o_ack,
   in_req => r_1_o_req,
   in_data => r_1_data,
   -- Output channel
   out_req => cl_2_o_req,
   out_data => cl_2_data,
   out_ack => cl_2_o_ack
  );
  
end beh_lb_0;
	architecture beh_cl_3 of funcBlock is
	alias x      : std_logic_vector(4 - 1 downto 0)  is in_data( 16 - 1 downto 12);
alias result : std_logic_vector(4 - 1 downto 0)  is out_data( 8 - 1 downto 4);

	  
    --attribute dont_touch : string;
	--attribute dont_touch of  x, y, result: signal is "true";
	  
    --attribute keep : boolean;
	--attribute keep of  x, y, result: signal is true;
  begin
    in_ack <= out_ack;
    
    delay_req: entity work.delay_element
      generic map(
        NUM_LCELLS => ADD_DELAY  -- Delay  size
      )
      port map (
        i => in_req,
        o => out_req
	  );
	  

	  calc: process(all)
	variable offset: integer range 0 to out_data'length;
	begin
	out_data <= in_data; 
	result <= std_logic_vector(resize(unsigned(x)  , result'length)) ;

	end process;
  end beh_cl_3;
  architecture beh_se_0 of Selector is
	alias x      : std_logic_vector(4 - 1 downto 0)  is in_data( 8 - 1 downto 4);
alias y      : std_logic_vector(4 - 1 downto 0)  is in_data( 4 - 1 downto 0);

  
    --attribute dont_touch : string;
	--attribute dont_touch of  x, y, selector: signal is "true";
	
	--attribute keep : boolean;
	--attribute keep of  x, y, selector: signal is true;
  begin
  
    delay_req: entity work.delay_element
      generic map(
        NUM_LCELLS => ADD_DELAY  -- Delay  size
      )
      port map (
        i => in_req,
        o => out_req
      );
  
    in_ack <= out_ack;
    
    selector(0) <= '0' when unsigned(x) /= to_unsigned(0, 4) else '1';


  end beh_se_0;
  architecture beh_lb_1 of LoopBlock is
	signal cl_4_o_req, cl_4_o_ack : std_logic;signal cl_4_data: std_logic_vector(DATA_WIDTH -1 downto 0);
signal mx_1_o_req, mx_1_o_ack : std_logic;signal mx_1_data: std_logic_vector(DATA_WIDTH -1 downto 0);
signal rf_1_b_o_req, rf_1_b_o_ack : std_logic;signal rf_1_b_data: std_logic_vector(DATA_WIDTH -1 downto 0);
signal rf_1_c_o_req, rf_1_c_o_ack : std_logic;signal rf_1_c_data: std_logic_vector(DATA_WIDTH -1 downto 0);
signal se_0_o_req, se_0_o_ack : std_logic;signal se_0_select: std_logic_vector(1-1 downto 0);
signal f_1_b_o_req, f_1_b_o_ack : std_logic;
signal f_1_c_o_req, f_1_c_o_ack : std_logic;
signal lb_1_o_req, lb_1_o_ack : std_logic;signal lb_1_data: std_logic_vector(DATA_WIDTH -1 downto 0);
signal dx_2_c_o_req, dx_2_c_o_ack : std_logic;signal dx_2_c_data: std_logic_vector(DATA_WIDTH -1 downto 0);
signal r_3_o_req, r_3_o_ack : std_logic;signal r_3_data: std_logic_vector(DATA_WIDTH -1 downto 0);
signal r_2_o_req, r_2_o_ack : std_logic;signal r_2_data : std_logic_vector(0 downto 0);

begin
out_req <= lb_1_o_req; 
lb_1_o_ack <= out_ack; 
out_data <= lb_1_data; 

MX_1: entity work.mux
  generic map (
    DATA_WIDTH => DATA_WIDTH
  )
  port map (
    inA_ack => in_ack,
    inA_data => in_data,
    inA_req => in_req,
    inB_ack => cl_4_o_ack,
    inB_data => cl_4_data,
    inB_req => cl_4_o_req,
    outC_ack => mx_1_o_ack,
    outC_data => mx_1_data,
    outC_req => mx_1_o_req,
    rst => rst,
    inSel_ack => r_2_o_ack,
    inSel_req => r_2_o_req,
    selector => r_2_data
  );
  
RF_1: entity work.reg_fork
  generic map(
    DATA_WIDTH => DATA_WIDTH,
    PHASE_INIT_A => '0',
    PHASE_INIT_B =>'0',
    PHASE_INIT_C => '0')
  port map (
    inA_ack => mx_1_o_ack,
    inA_data => mx_1_data,
    inA_req => mx_1_o_req,
    outB_ack => rf_1_b_o_ack,
    outB_data => rf_1_b_data,
    outB_req => rf_1_b_o_req,
    outC_ack => rf_1_c_o_ack,
    outC_data=> rf_1_c_data,
    outC_req => rf_1_c_o_req,
    rst => rst
  );
SE_0: entity work.Selector(beh_se_0)
	generic map(
	  DATA_WIDTH => DATA_WIDTH
	)
	port map (
	  -- Input channel
	  in_req  => rf_1_b_o_req,
	  in_ack  => rf_1_b_o_ack, 
	  in_data => rf_1_b_data,
	  -- Output channel
	  out_req => se_0_o_req,
	  out_ack => se_0_o_ack,
	  selector  => se_0_select
	);
F_1: entity work.fork
  generic map(
    DATA_WIDTH => 1,
    PHASE_INIT => '0'
  )
  port map(
    inA_ack => se_0_o_ack,
    inA_req => se_0_o_req,
    inA_data => se_0_select,
    -- Output Channel
    outB_ack => f_1_b_o_ack,
    outB_req => f_1_b_o_req,
    outB_data => open,
    outC_ack => f_1_c_o_ack,
    outC_req => f_1_c_o_req,
    outC_data => open,
    rst => rst
  );
    
R_2: entity work.decoupled_hs_reg(behavioural)
  generic map (
    DATA_WIDTH => 1,
    VALUE => 1,
    PHASE_INIT_IN => '0',
    PHASE_INIT_OUT => '1'
  )
  port map (
    rst => rst,
    in_ack => f_1_b_o_ack,
    in_req => f_1_b_o_req,
    in_data => se_0_select,
    -- Output channel
    out_req => r_2_o_req,
    out_data => r_2_data,
    out_ack => r_2_o_ack
  );
  
DX_2: entity work.demux
  generic map (
    DATA_WIDTH => DATA_WIDTH
  )
  port map (
    inA_ack => rf_1_c_o_ack,
    inA_data => rf_1_c_data,
    inA_req => rf_1_c_o_req,
    outB_ack => lb_1_o_ack,
    outB_data => lb_1_data,
    outB_req => lb_1_o_req,
    outC_ack => dx_2_c_o_ack,
    outC_data => dx_2_c_data,
    outC_req => dx_2_c_o_req,
    rst => rst,
    inSel_ack => f_1_c_o_ack,
    inSel_req => f_1_c_o_req,
    selector => se_0_select
  );
   
R_3: entity work.decoupled_hs_reg(behavioural)
  generic map (
    DATA_WIDTH => DATA_WIDTH,
    VALUE => 0,
    PHASE_INIT_IN => '0',
    PHASE_INIT_OUT => '0'
  )
  port map (
    rst => rst,
    in_ack => dx_2_c_o_ack,
    in_req => dx_2_c_o_req,
    in_data => dx_2_c_data,
    -- Output channel
    out_req => r_3_o_req,
    out_data => r_3_data,
    out_ack => r_3_o_ack
  );
  
B_1: entity work.BlockC(beh_b_1)
  generic map(
   DATA_WIDTH => DATA_WIDTH
  )
  port map (
   rst => rst,
   in_ack => r_3_o_ack,
   in_req => r_3_o_req,
   in_data => r_3_data,
   -- Output channel
   out_req => cl_4_o_req,
   out_data => cl_4_data,
   out_ack => cl_4_o_ack
  );
  
end beh_lb_1;
	architecture beh_b_0 of BlockC is
	signal r_4_o_req, r_4_o_ack : std_logic;signal r_4_data: std_logic_vector(DATA_WIDTH -1 downto 0);
signal lb_1_o_req, lb_1_o_ack : std_logic;signal lb_1_data: std_logic_vector(DATA_WIDTH -1 downto 0);

begin
out_req <= lb_1_o_req; 
lb_1_o_ack <= out_ack; 
out_data <= lb_1_data; 

R_4: entity work.decoupled_hs_reg(behavioural)
  generic map (
    DATA_WIDTH => DATA_WIDTH,
    VALUE => 0,
    PHASE_INIT_IN => '0',
    PHASE_INIT_OUT => '0'
  )
  port map (
    rst => rst,
    in_ack => in_ack,
    in_req => in_req,
    in_data => in_data,
    -- Output channel
    out_req => r_4_o_req,
    out_data => r_4_data,
    out_ack => r_4_o_ack
  );
  LB_1: entity work.LoopBlock(beh_lb_1)
  generic map(
    DATA_WIDTH => DATA_WIDTH
  )
  port map (
    rst => rst,
    in_ack => r_4_o_ack,
    in_req => r_4_o_req,
    in_data => r_4_data,
    -- Output channel
    out_req => lb_1_o_req,
    out_data => lb_1_data,
    out_ack => lb_1_o_ack
  );
  end beh_b_0;
	architecture beh_cl_0 of funcBlock is
	alias x      : std_logic_vector(4 - 1 downto 0)  is in_data( 8 - 1 downto 4);
alias result : std_logic_vector(4 - 1 downto 0)  is out_data( 12 - 1 downto 8);

	  
    --attribute dont_touch : string;
	--attribute dont_touch of  x, y, result: signal is "true";
	  
    --attribute keep : boolean;
	--attribute keep of  x, y, result: signal is true;
  begin
    in_ack <= out_ack;
    
    delay_req: entity work.delay_element
      generic map(
        NUM_LCELLS => ADD_DELAY  -- Delay  size
      )
      port map (
        i => in_req,
        o => out_req
	  );
	  

	  calc: process(all)
	variable offset: integer range 0 to out_data'length;
	begin
	out_data <= in_data; 
	result <= std_logic_vector(resize(unsigned(x)  , result'length)) ;

	end process;
  end beh_cl_0;
  architecture beh_cl_1 of funcBlock is
	alias x      : std_logic_vector(4 - 1 downto 0)  is in_data( 4 - 1 downto 0);
alias result : std_logic_vector(4 - 1 downto 0)  is out_data( 16 - 1 downto 12);

	  
    --attribute dont_touch : string;
	--attribute dont_touch of  x, y, result: signal is "true";
	  
    --attribute keep : boolean;
	--attribute keep of  x, y, result: signal is true;
  begin
    in_ack <= out_ack;
    
    delay_req: entity work.delay_element
      generic map(
        NUM_LCELLS => ADD_DELAY  -- Delay  size
      )
      port map (
        i => in_req,
        o => out_req
	  );
	  

	  calc: process(all)
	variable offset: integer range 0 to out_data'length;
	begin
	out_data <= in_data; 
	result <= std_logic_vector(resize(unsigned(x)  , result'length)) ;

	end process;
  end beh_cl_1;
  architecture GCD of Scope is
	signal lb_1_o_req, lb_1_o_ack : std_logic;signal lb_1_data: std_logic_vector(DATA_WIDTH -1 downto 0);
signal r_5_o_req, r_5_o_ack : std_logic;signal r_5_data: std_logic_vector(DATA_WIDTH -1 downto 0);

begin
out_req <= r_5_o_req; 
r_5_o_ack <= out_ack; 
out_data <= r_5_data(4 -1 downto 0);

B_0: entity work.BlockC(beh_b_0)
  generic map(
   DATA_WIDTH => DATA_WIDTH
  )
  port map (
   rst => rst,
   in_ack => in_ack,
   in_req => in_req,
   in_data => std_logic_vector(resize(unsigned(in_data), DATA_WIDTH)),
   -- Output channel
   out_req => lb_1_o_req,
   out_data => lb_1_data,
   out_ack => lb_1_o_ack
  );
  
R_5: entity work.decoupled_hs_reg(behavioural)
  generic map (
    DATA_WIDTH => DATA_WIDTH,
    VALUE => 0,
    PHASE_INIT_IN => '0',
    PHASE_INIT_OUT => '0'
  )
  port map (
    rst => rst,
    in_ack => lb_1_o_ack,
    in_req => lb_1_o_req,
    in_data => lb_1_data,
    -- Output channel
    out_req => r_5_o_req,
    out_data => r_5_data,
    out_ack => r_5_o_ack
  );
  
end GCD;
	