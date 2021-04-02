
--------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;
---------------------------------------------------------

ENTITY processor_top IS
  PORT(
    -- clock and reset
    clk: IN std_logic;
    rst: IN std_logic;
    
    -- Data Interface
    data_in: IN std_logic_vector(15 DOWNTO 0);
    data_out: OUT std_logic_vector(15 DOWNTO 0));

END processor_top;

ARCHITECTURE PROCESSOR1 OF processor_top IS

COMPONENT decode_control
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
    
END COMPONENT;

COMPONENT inst_fetch 
  PORT (
    -- clock and reset
    clk: IN std_logic;
    rst: IN std_logic;
    
    -- Read Interface memory Interface
    rd_inst_en: OUT std_logic;
    PC: OUT std_logic_vector (3 DOWNTO 0);
    read_data_in: IN std_logic_vector (9 DOWNTO 0);

    -- Prcessing Interface
    IRLOAD: IN std_logic;
    IR: OUT std_logic_vector (9 DOWNTO 0);
    fetch_done: OUT std_logic;


    -- PC Load Interface
    PCload: IN std_logic;
    jump: IN std_logic);
  
END COMPONENT;

COMPONENT ROM
  PORT(
    -- clock and reset
    clk: IN std_logic;
    rst: IN std_logic;
    
    -- Read Interface memory Interface
    rd_inst_en: IN std_logic;
    PC: in std_logic_vector (3 DOWNTO 0);
    read_data_out: out std_logic_vector (9 DOWNTO 0));

END COMPONENT;

SIGNAL rd_inst_en: std_logic;
SIGNAL PC: std_logic_vector (3 DOWNTO 0);
SIGNAL istruction_read_out: std_logic_vector (9 DOWNTO 0);

SIGNAL IRLOAD: std_logic;
SIGNAL IR: std_logic_vector (9 DOWNTO 0);
SIGNAL PCload: std_logic;
SIGNAL jump: std_logic;
SIGNAL fetch_done_map: std_logic;

BEGIN

  ProgramROM: ROM PORT MAP (
    -- clock and reset
    clk => clk,
    rst => rst,
    
    -- Read Interface memory Interface
    --rd_inst_en => rd_inst_en,
    rd_inst_en => '1',
    PC => PC,
    read_data_out => istruction_read_out);

  Fetch_engine: inst_fetch PORT MAP(
    -- clock and reset
    clk => clk,
    rst => rst,
    
    -- Read Interface memory Interface
    rd_inst_en => rd_inst_en,
    PC => PC,
    read_data_in => istruction_read_out,

    -- Prcessing Interface
    IRLOAD => IRLOAD,
    IR => IR,
    fetch_done => fetch_done_map,

    -- PC Load Interface
    PCload => PCload,
    jump => jump);

  process_engine: decode_control PORT MAP (
    -- clock and reset
    clk => clk,
    rst => rst,
    
    -- control out 
    PCload => PCload,
    IRload => IRLoad,
    JUMP => jump,

    -- Data Interface
    data_in => data_in,
    data_out => data_out,

    -- Instruction in
    fetch_done => fetch_done_map,
    IR => IR); -- Instruction fetched 
  
END PROCESSOR1;
  
