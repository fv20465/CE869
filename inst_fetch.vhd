LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY inst_fetch IS
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
  
END inst_fetch;

ARCHITECTURE A1 OF inst_fetch IS

  SIGNAL IR_INT: std_logic_vector (9 DOWNTO 0);
  SIGNAL addr_int: std_logic_vector (3 DOWNTO 0);
  SIGNAL addr_inc: std_logic_vector (3 DOWNTO 0);
  SIGNAL PC_int: std_logic_vector (3 DOWNTO 0);
  SIGNAL read_en: std_logic;
  SIGNAL fetch_done_int: std_logic;
  
  
BEGIN

  -- Loading instruction from memory
  PROCESS(clk, rst)
  BEGIN
    if(rst='1') THEN
      IR_INT <= "0000000000";
    elsif(clk'event AND clk='1') THEN
      IF(IRLOAD = '1') THEN
        IR_INT <= read_data_in;
      END IF;
    END IF;
  END PROCESS;

  -- Asigning current instruction to output
  IR <= IR_INT;
  
  -- Program address mux
  PROCESS (clk, rst)
  BEGIN
    if(rst = '1') THEN
      addr_int <= (OTHERS => '0');
    ELSIF (clk'event AND clk='1') THEN 
      IF(jump = '1') THEN
        addr_int <= IR_INT(3 DOWNTO 0);
      ELSE
        addr_int <= addr_inc;
      END IF;
    END IF;
  END PROCESS;
  
  -- WITH jump select
  --   addr_int <= IR_INT(3 DOWNTO 0) WHEN '1',
  --               addr_inc           WHEN OTHERS;

  -- address increment
  addr_inc <= PC_int+'1';
  
  -- PC Loading
  PROCESS (clk, rst)
  BEGIN
    if(rst = '1') THEN
      PC_int <= "0000";
      fetch_done_int <='0';
    elsif(clk'event AND clk='1') THEN
      --IF(PCload='1') THEN
      IF(PCload='1') THEN
        PC_int <= addr_int;
        fetch_done_int <= '1';
      ELSE
        fetch_done_int <= '0';        
      END IF;
    END IF;
  END PROCESS;

  fetch_done <= fetch_done_int;
  
  -- PC assignment for reading next instruction from memory
  PC <= PC_int;
  
  -- read enable generation for memory (once previous instruction finished operation)
  PROCESS (clk, rst)
  BEGIN
    if(rst = '1') THEN
      read_en <= '0';
    elsif(clk'event AND clk='1') THEN
      IF(PCload = '1') THEN
        read_en <= '1';
      ELSE
        read_en <= '0';
      END IF;
    END IF;
  END PROCESS;

  rd_inst_en <= read_en;
  
END A1;
