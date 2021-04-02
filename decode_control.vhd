
--------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;
---------------------------------------------------------

ENTITY decode_control IS
  PORT (
    -- clock and reset
    clk: IN std_logic;
    rst: IN std_logic;
    
    -- control out 
    PCload: OUT std_logic;
    IRload: OUT std_logic;
    JUMP: OUT std_logic;

    -- Data Interface
    data_in: IN std_logic_vector(15 DOWNTO 0);
    data_out: OUT std_logic_vector(15 DOWNTO 0);

    -- Instruction in
    fetch_done: IN std_logic;
    IR: IN std_logic_vector(9 DOWNTO 0)); -- Instruction fetched 
    
END decode_control;

ARCHITECTURE CD1 OF decode_control IS

  -- State Code
  CONSTANT IDLE   : std_logic_vector(1 DOWNTO 0):= "00"; -- System in reset
  CONSTANT FETCH  : std_logic_vector(1 DOWNTO 0):= "01"; -- Fetch the new instruction
  CONSTANT DECODE : std_logic_vector(1 DOWNTO 0):= "10"; -- Decode the instruction
  CONSTANT EXECUTE: std_logic_vector(1 DOWNTO 0):= "11"; -- Execute the instruction

  -- Present State
  SIGNAL ps: std_logic_vector(1 DOWNTO 0);
  -- next State
  SIGNAL ns: std_logic_vector(1 DOWNTO 0);

  -- command from IR
  SIGNAL command : std_logic_vector(3 DOWNTO 0);
  
  -- opcode control
  SIGNAL opcode_int: std_logic_vector(2 DOWNTO 0);

  -- Load data
  SIGNAL reg_load_data: std_logic_vector(15 DOWNTO 0);

  -- load enable signals
  SIGNAL load_aa_en: std_logic;
  SIGNAL load_bb_en: std_logic;
  SIGNAL load_cc_en: std_logic;
  SIGNAL load_dd_en: std_logic;

  -- output loading
  SIGNAL load_output_en: std_logic;
  
  -- internal registers
  SIGNAL RAA: std_logic_vector(15 DOWNTO 0);
  SIGNAL RBB: std_logic_vector(15 DOWNTO 0);
  SIGNAL RCC: std_logic_vector(15 DOWNTO 0);
  SIGNAL RDD: std_logic_vector(15 DOWNTO 0);
  SIGNAL OUTREG: std_logic_vector(15 DOWNTO 0);

  -- processing signals
  SIGNAL data1: std_logic_vector(15 DOWNTO 0);
  SIGNAL data2: std_logic_vector(15 DOWNTO 0);

  -- Intermidiate prossing signals
  SIGNAL data_not: std_logic_vector(15 DOWNTO 0);
  SIGNAL data_add: std_logic_vector(15 DOWNTO 0);
  SIGNAL data_sub: std_logic_vector(15 DOWNTO 0);
  SIGNAL data_and: std_logic_vector(15 DOWNTO 0);
  SIGNAL data_OR: std_logic_vector(15 DOWNTO 0);

  SIGNAL calc_data: std_logic_vector(15 DOWNTO 0);
  
  SIGNAL exec:std_logic;
  
  -- Processign element declerations
  signal source1_sel: std_logic_vector(3 DOWNTO 0);
  signal source2_sel: std_logic_vector(3 DOWNTO 0);
  signal dest_sel: std_logic_vector(3 DOWNTO 0);
  SIGNAL comm_val: std_logic;
  SIGNAL Z_Check: std_logic;
  SIGNAL Z_check_val: std_logic;
  SIGNAL Z_val_eval: std_logic;
  SIGNAL Z:std_logic;
          
  -- PC Loading
  SIGNAL PCload_int:std_logic;
  SIGNAL PCload_int2: std_logic;

  -- JUMP Signal
  SIGNAL jump_int:std_logic;
  
  -- IR Loading
  SIGNAL IRload_int:std_logic;
  
BEGIN

  --------------------------------------------------------
  -- Control FSM
  --------------------------------------------------------

  -- FSM Flops
  PROCESS(clk, rst)
  BEGIN
    if(rst = '1') THEN
      ps <= IDLE;
    elsif(clk'event AND clk='1') THEN
      ps <= ns;
    END if;
  END PROCESS;

  --------------------------------------------------------
  -- next state decoder
  --------------------------------------------------------
  PROCESS(ps,fetch_done)
  BEGIN
    CASE ps IS
      WHEN IDLE => 
        ns <= FETCH;
      WHEN FETCH =>
        IF(fetch_done='1')THEN 
          ns <= DECODE;
        ELSE
          ns <= FETCH;
        END IF;
      WHEN DECODE =>
        ns <= EXECUTE;
      WHEN EXECUTE =>
        ns <= FETCH;
      WHEN OTHERS =>
        ns <= IDLE;
    END CASE;
  END PROCESS;
  

  --------------------------------------------------------
  -- Instruction decode
  --------------------------------------------------------

  -- command extraction for execution
  command <= IR(9 DOWNTO 6);
  
  PROCESS(clk, rst)
  BEGIN
    IF(rst='1')THEN
          opcode_int <= (OTHERS => '0'); 
          source1_sel <= (OTHERS => '0');
          source2_sel <= (OTHERS => '0');
          dest_sel <= (OTHERS => '0');
          comm_val <= '0';
          PCload_int <= '0';
          Z_Check <= '0';
          Z_check_val <= '0';
          Z_val_eval <= '0';
    elsif(clk'event AND clk='1') THEN
        -- opcode_int <= (OTHERS => '0'); 
        -- source1_sel <= (OTHERS => '0');
        -- source2_sel <= (OTHERS => '0');
        -- dest_sel <= (OTHERS => '0');
        -- comm_val <= '0';
        -- PCload_int <= '0';
        -- Z_Check <= '0';
        -- Z_check_val <= '0';
        -- Z_val_eval <= '0';

        IF (ps = DECODE) THEN 

            CASE command IS
              
              WHEN "0001" => -- MOV source1 to destination
                  opcode_int <= "000"; -- MOV/ load
                  source1_sel <= "00" & IR(3 DOWNTO 2);
                  dest_sel <= "00" & IR(1 DOWNTO 0);
                  comm_val <= '1';

              WHEN "0010" => -- load input to dd
                  opcode_int <= "000"; -- load input to dd
                  source1_sel <= "0100"; -- input port address
                  dest_sel <= "00" & IR(1 DOWNTO 0);
                  comm_val <= '1';

              WHEN "0011" => -- drive ss to output
                  opcode_int <= "000"; -- load input to dd
                  source1_sel <= "00" & IR(1 DOWNTO 0);
                  dest_sel <= "1111"; -- Output port address
                  comm_val <= '1';

              WHEN "0100" => -- load not of ss to dd
                  opcode_int <= "001"; -- load not of ss to dd
                  source1_sel <= "00" & IR(1 DOWNTO 0); -- input reg
                  dest_sel <= "00" & IR(3 DOWNTO 2); -- Output reg address
                  comm_val <= '1';

              WHEN "0101" => -- jump to aaaa in program counter
                  opcode_int <= "010"; -- jump to aaaa in program counter
                  comm_val <= '1';
                  PCload_int <= '1';
                  Z_Check <= '0';

              WHEN "0110" => -- jump to aaaa if z is zero
                  opcode_int <= "010"; -- jump to aaaa in program counter
                  comm_val <= '1';
                  PCload_int <= '1';
                  Z_Check <= '1';
                  Z_check_val <= '0';

              WHEN "0111" => -- jump to aaaa if z is one
                  opcode_int <= "010"; -- jump to aaaa in program counter
                  comm_val <= '1';
                  PCload_int <= '1';
                  Z_Check <= '1';
                  Z_check_val <= '1';

              WHEN "1000" => -- set z to one if Rrr < Rqq
                  opcode_int <= "000"; -- set z to one if Rrr < Rqq
                  source1_sel <= "00" & IR(3 DOWNTO 2); -- RR Select
                  source2_sel <= "00" & IR(1 DOWNTO 0); -- QQ Select
                  dest_sel <= "00" & IR(3 DOWNTO 2); -- RR Select
                  comm_val <= '1';
                  Z_val_eval <= '1';

              WHEN "1001" => -- INC Rrr with NNNN
                  opcode_int <= "011"; -- INC Rrr with NNNN
                  source1_sel <= "00" & IR(5 DOWNTO 4); -- RR Select
                  source2_sel <= "1000"; -- Command buffer 3 downto 0 select as NNNN 
                  dest_sel <= "00" & IR(5 DOWNTO 4); -- Output reg address
                  comm_val <= '1';

              WHEN "1010" => -- DEC Rrr with NNNN
                  opcode_int <= "100"; -- DEC Rrr with NNNN
                  source1_sel <= "00" & IR(5 DOWNTO 4); -- RR Select
                  source2_sel <= "1000"; -- Command buffer 3 downto 0 select as NNNN 
                  dest_sel <= "00" & IR(5 DOWNTO 4); -- Output reg address
                  comm_val <= '1';

              WHEN "1011" => -- ADD Rrr + Rqq => Rdd
                  opcode_int <= "011"; -- ADD Rrr + Rqq => Rdd
                  source1_sel <= "00" & IR(3 DOWNTO 2); -- RR Select
                  source2_sel <= "00" & IR(1 DOWNTO 0); -- QQ Select  
                  dest_sel <= "00" & IR(5 DOWNTO 4); -- Output reg address dd
                  comm_val <= '1';

              WHEN "1100" => -- SUB Rrr - Rqq => Rdd
                  opcode_int <= "100"; -- ADD Rrr - Rqq => Rdd
                  source1_sel <= "00" & IR(3 DOWNTO 2); -- RR Select
                  source2_sel <= "00" & IR(1 DOWNTO 0); -- QQ Select  
                  dest_sel <= "00" & IR(5 DOWNTO 4); -- Output reg address dd
                  comm_val <= '1';

              WHEN "1101" => -- Rrr AND Rqq => Rdd
                  opcode_int <= "101"; -- Rrr AND Rqq => Rdd
                  source1_sel <= "00" & IR(3 DOWNTO 2); -- RR Select
                  source2_sel <= "00" & IR(1 DOWNTO 0); -- QQ Select  
                  dest_sel <= "00" & IR(5 DOWNTO 4); -- Output reg address dd
                  comm_val <= '1';

              WHEN "1110" => -- Rrr OR Rqq => Rdd
                  opcode_int <= "110"; -- Rrr OR Rqq => Rdd
                  source1_sel <= "00" & IR(3 DOWNTO 2); -- RR Select
                  source2_sel <= "00" & IR(1 DOWNTO 0); -- QQ Select  
                  dest_sel <= "00" & IR(5 DOWNTO 4); -- Output reg address dd
                  comm_val <= '1';

              WHEN "1111" => -- nnnn => Rdd
                  opcode_int <= "000"; -- nnnn => Rdd
                  source1_sel <= "1000"; -- nnnn Select
                  dest_sel <= "00" & IR(5 DOWNTO 4); -- Output reg address dd
                  comm_val <= '1';

              WHEN OTHERS => -- Halt state
                    opcode_int <= (OTHERS => '0'); 
                    source1_sel <= (OTHERS => '0');
                    source2_sel <= (OTHERS => '0');
                    dest_sel <= (OTHERS => '0');
                    comm_val <= '0';
                    PCload_int <= '0';
                    Z_Check <= '0';
                    Z_check_val <= '0';

            END CASE;
           END IF;
      END IF;
  END PROCESS;

  -----------------------------------------------------------------
  -- Opcode based decoding units
  -----------------------------------------------------------------
  -- Mov opcode = 000
  WITH source1_sel select
    data1 <= RAA WHEN "0000",
             RBB WHEN "0001",
             RCC WHEN "0010",
             RDD WHEN "0011",
             data_in WHEN "0100",
             (( 15 DOWNTO 4 =>'0')&IR(3 DOWNTO 0)) WHEN "1000",
             (OTHERS => '0') WHEN others;
  
  WITH source2_sel select
    data2 <= RAA WHEN "0000",
             RBB WHEN "0001",
             RCC WHEN "0010",
             RDD WHEN "0011",
             data_in WHEN "0100",
             ((15 DOWNTO 4 =>'0')&IR(3 DOWNTO 0)) WHEN "1000",
             (OTHERS => '0') WHEN others;
  
  -----------------------------------------------------------------
  -- Control signal generation
  -----------------------------------------------------------------
  WITH dest_sel select
    load_aa_en <= comm_val WHEN "0000", '0' WHEN others;
  
  WITH dest_sel select
    load_bb_en <= comm_val WHEN "0001", '0' WHEN others;
  
  WITH dest_sel select
    load_cc_en <= comm_val WHEN "0010", '0' WHEN others;
  
  WITH dest_sel select
    load_dd_en <= comm_val WHEN "0011", '0' WHEN others;
  
  WITH dest_sel select
    load_output_en <= comm_val WHEN "1111", '0' WHEN others;

  -----------------------------------------------------------------
  -- processing Element Calculations
  -----------------------------------------------------------------
  data_not <= NOT data1;
  data_add <= data1 + data2;
  data_sub <= data1 - data2;
  data_and <= data1 AND data2;
  data_or <= data1 OR data2;
  
  -----------------------------------------------------------------
  -- Selecting the right path
  -----------------------------------------------------------------
  WITH opcode_int select
    calc_data <= data1 WHEN "000",
                 data_not WHEN "001",
                 data_add WHEN "011",
                 data_sub WHEN "100",
                 data_and WHEN "101",
                 data_or  WHEN "110",
                 (OTHERS =>'0') WHEN others;

  -----------------------------------------------------------------
  -- sending data to specific registers
  -----------------------------------------------------------------
  reg_load_data <= calc_data;
  
  -----------------------------------------------------------------
  -- Z Evaluation for internal opcode 011
  -----------------------------------------------------------------
  -- process stage check
  WITH ps SELECT 
  exec <= '1' WHEN EXECUTE, '0' WHEN OTHERS;
  
  PROCESS (clk, rst)
  BEGIN
    if(rst = '1') THEN
      Z <= '0';
    elsif(clk'event AND clk='1') THEN
      if((Z_val_eval = '1')AND(exec = '1')AND(data1 < data2)) THEN 
          Z <= '1';
        END IF;
    END IF;
  END PROCESS;
    
  -----------------------------------------------------------------
  -- Execution extraction
  -----------------------------------------------------------------

  -----------------------------------------------------------------
  -- Internal data processing buffers
  -----------------------------------------------------------------
  -- Reg AA, Address 00
  PROCESS (clk, rst)
  BEGIN
    if(rst = '1') THEN
      RAA <= (OTHERS => '0');
    elsif(clk'event AND clk='1') THEN
      if((load_aa_en='1')AND(exec = '1')) THEN 
        RAA <= reg_load_data;
      END if;
    END if;
  END PROCESS;
  
  -- Reg BB, Address 01
  PROCESS (clk, rst)
  BEGIN
    if(rst = '1') THEN
      RBB <= (OTHERS => '0');
    elsif(clk'event AND clk='1') THEN
      if((load_bb_en='1') AND (exec = '1')) THEN 
        RBB <= reg_load_data;
      END if;
    END if;
  END PROCESS;
  
  -- Reg CC, Address 10
  PROCESS (clk, rst)
  BEGIN
    if(rst = '1') THEN
      RCC <= (OTHERS => '0');
    elsif(clk'event AND clk='1') THEN
      if((load_cc_en = '1') AND (exec = '1')) THEN
        RCC <= reg_load_data;
      END if;
    END if;
  END PROCESS;
  
  -- Reg DD, Address 11
  PROCESS (clk, rst)
  BEGIN
    if(rst = '1') THEN
      RDD <= (OTHERS => '0');
    elsif(clk'event AND clk='1') THEN
      if((load_dd_en='1') AND (exec = '1')) THEN
        RDD <= reg_load_data;
      END if;
    END if;
  END PROCESS;

  -----------------------------------------------------------------
  -- Reg output, Address 1111
  -----------------------------------------------------------------
  PROCESS (clk, rst)
  BEGIN
    if(rst = '1') THEN
      OUTREG <= (OTHERS => '0');
    elsif(clk'event AND clk='1') THEN
      if((load_output_en='1') AND (exec = '1')) THEN
        OUTREG <= reg_load_data;
      END if;
    END if;
  END PROCESS;

 data_out <= OUTREG;

  --------------------------------------------------------------------
  -- IRload
  --------------------------------------------------------------------
  PROCESS (clk, rst)
  BEGIN
    if(rst = '1') THEN
      IRload_int <= '0';
    elsif(clk'event AND clk='1') THEN
      if(ps=FETCH) THEN
        IRload_int <= '1';
      ELSE 
        IRload_int <= '0';        
      END if;
    END if;
  END PROCESS;

  IRload <= IRload_int;


  --------------------------------------------------------------------
  -- PCload control
  --------------------------------------------------------------------
  PROCESS (clk, rst)
  BEGIN
    if(rst = '1') THEN
      PCload_int2 <= '0';
    elsif(clk'event AND clk='1') THEN
      --if((ps=EXECUTE)AND(PCload_int='1'))THEN
      if((ps=FETCH))THEN
        PCload_int2 <= '1';
      ELSE
        PCload_int2 <= '0';        
      END if;
    END if;
  END PROCESS;

  PCload <= PCload_int2;

  --------------------------------------------------
  -- JUMP
  --------------------------------------------------
  PROCESS (clk, rst)
  BEGIN
    if(rst = '1') THEN
      jump_int <= '0';
    elsif(clk'event AND clk='1') THEN
      if((ps=EXECUTE)AND(PCload_int='1'))THEN
        if(((Z_Check='1')AND(Z=Z_check_val))OR(Z_Check='0')) THEN 
             jump_int <= '1';
           END if;
      ELSE
        jump_int <= '0';        
      END if;
    END if;
  END PROCESS;

  JUMP <= jump_int;

  
END CD1;
