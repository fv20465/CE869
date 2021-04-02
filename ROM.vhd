LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY ROM IS
  PORT(
    -- clock and reset
    clk: IN std_logic;
    rst: IN std_logic;
    
    -- Read Interface memory Interface
    rd_inst_en: IN std_logic;
    PC: in std_logic_vector (3 DOWNTO 0);
    read_data_out: out std_logic_vector (9 DOWNTO 0));

END ROM;

ARCHITECTURE ROM1 OF ROM IS
  
BEGIN

  -- PROCESS (clk, rst)
  -- BEGIN
  --   if(rst='1') THEN
  --     read_data_out <= "0000000000"; -- HALT
  --   ELSIF(clk'event AND clk='1') THEN 

  PROCESS (PC, rd_inst_en)
    BEGIN
        IF(rd_inst_en='1') THEN 
          CASE PC IS
            WHEN "0000" => read_data_out <= "0010000000"; -- memory Location 0
                                                          -- Reading inputs
                                                          -- from Input and
                                                          -- putting it into
                                                          -- reg AA
            WHEN "0001" => read_data_out <= "0000000000"; -- NOP memory Location 0
            WHEN "0010" => read_data_out <= "0000000000"; -- NOP memory Location 0
            WHEN "0011" => read_data_out <= "0000000000"; -- NOP memory Location 0
            WHEN "0100" => read_data_out <= "1001001111"; -- memory Location 0
                                                          -- Adding F in AA
                                                          -- Register 
            WHEN "0101" => read_data_out <= "0000000000"; -- NOP memory Location 0
            WHEN "0110" => read_data_out <= "0000000000"; -- NOP memory Location 0
            WHEN "0111" => read_data_out <= "0000000000"; -- NOP memory Location 0
            WHEN "1000" => read_data_out <= "0011000000"; -- NOP memory Location 0
                                                          -- Output Data AA to
                                                          -- output.
            WHEN "1001" => read_data_out <= "0000000000"; -- NOP memory Location 0
            WHEN "1010" => read_data_out <= "0000000000"; -- NOP memory Location 0
            WHEN "1011" => read_data_out <= "0000000000"; -- NOP memory Location 0
            WHEN "1100" => read_data_out <= "0000000000"; -- NOP memory Location 0
            WHEN "1101" => read_data_out <= "0000000000"; -- NOP memory Location 0
            WHEN "1110" => read_data_out <= "0000000000"; -- NOP memory Location 0
            -- WHEN "1111" => read_data_out <= "0000000000"; -- memory Location 0
            WHEN others => read_data_out <= "0000000000"; -- NOP memory Location 0
           END CASE;
        ELSE
          read_data_out <= "0000000000"; -- HALT
        --END IF;
      --END
        END IF;
                           
  END PROCESS;

END ROM1;
