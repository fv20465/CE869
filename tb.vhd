
--------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;
---------------------------------------------------------

ENTITY tb IS
END tb;

ARCHITECTURE Test1 OF tb IS

  COMPONENT processor_top
  PORT(
    -- clock and reset
    clk: IN std_logic;
    rst: IN std_logic;
    
    -- Data Interface
    data_in: IN std_logic_vector(15 DOWNTO 0);
    data_out: OUT std_logic_vector(15 DOWNTO 0));

  END COMPONENT;

  SIGNAL sim_clk: std_logic;
  SIGNAL sim_rst: std_logic;
  CONSTANT clk_period : time := 10 ns;
  
  SIGNAL data_feed: std_logic_vector(15 DOWNTO 0);
  SIGNAL data_received: std_logic_vector(15 DOWNTO 0);
  
BEGIN


clk_process :PROCESS 
   BEGIN 
        sim_clk <= '0';
        WAIT FOR clk_period/2;  --for 0.5 ns signal is '0'.
        sim_clk <= '1';
        WAIT FOR clk_period/2;  --for next 0.5 ns signal is '1'.
   END PROCESS;
  
rst_process :PROCESS 
   BEGIN 
        sim_rst <= '1';
        WAIT FOR clk_period*10;  --for 0.5 ns signal is '0'.
        sim_rst <= '0';
        WAIT FOR clk_period*100000;  --for next 0.5 ns signal is '1'.
        WAIT;
   END PROCESS;
  
proc: processor_top PORT MAP(
    -- clock and reset
    clk => sim_clk,
    rst => sim_rst,
    
    -- Data Interface
    data_in => data_feed,
    data_out => data_received);

  PROCESS(sim_clk, sim_rst)
  BEGIN
    if(sim_rst='1') THEN
      data_feed <= (OTHERS => '0');
    elsif(sim_clk'event AND sim_clk='1') THEN
      data_feed <= data_feed + '1';
    END IF;
  END PROCESS;

END Test1;

      
   
   
