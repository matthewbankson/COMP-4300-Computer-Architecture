-- dlx_datapath.vhd

package dlx_types is
  subtype dlx_word is bit_vector(31 downto 0); 
  subtype half_word is bit_vector(15 downto 0); 
  subtype byte is bit_vector(7 downto 0); 

  subtype alu_operation_code is bit_vector(3 downto 0); 
  subtype error_code is bit_vector(3 downto 0); 
  subtype register_index is bit_vector(4 downto 0);

  subtype opcode_type is bit_vector(5 downto 0);
  subtype offset26 is bit_vector(25 downto 0);
  subtype func_code is bit_vector(5 downto 0);
end package dlx_types; 

library work;
use work.dlx_types.all; 
use work.bv_arithmetic.all;  

entity alu is 
     port(operand1, operand2: in dlx_word; operation: in alu_operation_code; 
          signed: in bit; result: out dlx_word; error: out error_code); 
end entity alu; 

architecture behaviour of alu is

begin

   alu_behav: process(operand1,operand2,operation,signed) is
      variable resultval: dlx_word;
      variable overflow_flag_set: boolean;
      variable less_than: boolean;
   
   begin
      error <= "0000";
      case(operation) is
         when "0000" =>
            if signed = '1' then
               bv_add(operand1, operand2, resultval, overflow_flag_set);
               if overflow_flag_set then
                  error <= "0001";
               end if;
               result <= resultval after 5 ns;
            else 
               bv_addu(operand1, operand2, resultval, overflow_flag_set);
               if overflow_flag_set then
                  error <= "0001";
               end if;
               result <= resultval after 5 ns;
            end if;
         when "0001" =>
            if signed = '1' then
               bv_sub(operand1, operand2, resultval, overflow_flag_set);
               if overflow_flag_set then
                  error <= "0001";
               end if;
               result <= resultval after 5 ns;
            else 
               bv_subu(operand1, operand2, resultval, overflow_flag_set);
               if overflow_flag_set then
                  error <= "0001";
               end if;
               result <= resultval after 5 ns;
            end if;
         when "0010" =>
            for i in 31 downto 0 loop
               resultval(i) := operand1(i) AND operand2(i);
            end loop;
            result <= resultval after 5 ns;
         when "0011" =>
            for i in 31 downto 0 loop
               resultval(i) := operand1(i) OR operand2(i);
            end loop;
            result <= resultval after 5 ns;
         when "1011" =>
            less_than := bv_lt(operand1, operand2);
            if less_than then
               result <= x"00000001" after 5 ns;
            else
               result <= x"00000000" after 5 ns;
            end if;
         when "1110" =>
            if signed = '1' then
               bv_mult(operand1, operand2, resultval, overflow_flag_set);
               if overflow_flag_set then
                  error <= "0001";
               end if;
               result <= resultval after 5 ns;
            else 
               bv_multu(operand1, operand2, resultval, overflow_flag_set);
               if overflow_flag_set then
                  error <= "0001";
               end if;
               result <= resultval after 5 ns;
            end if;
         when others =>
            result <= x"00000000" after 5 ns;
      end case;
   end process alu_behav;
end architecture behaviour;


use work.dlx_types.all; 

entity mips_zero is
  
  port (
    input  : in  dlx_word;
    output : out bit);

end mips_zero;

architecture behaviour of mips_zero is
begin
	zero: process(input)
	begin
		if(input = "00000000000000000000000000000000") then
			output <= '1' after 5 ns;
		else
			output <= '0' after 5 ns;
		end if;
	end process zero;
end architecture behaviour;

  
library work;
use work.dlx_types.all;
use work.bv_arithmetic.all;

entity mips_register is
     port(in_val: in dlx_word; clock: in bit; out_val: out dlx_word);
end entity mips_register;

architecture behaviour of mips_register is
begin
	reg_behav: process(in_val,clock)
	variable value : dlx_word := B"00000000000000000000000000000000";
   begin
      if (clock = '1') then
         value := in_val;
      end if;
      out_val <= value after 5 ns;
   end process reg_behav;

end architecture behaviour;

library work;
use work.dlx_types.all; 
use work.bv_arithmetic.all;

entity mips_bit_register is
     port(in_val: in bit; clock: in bit; out_val: out bit);
end entity mips_bit_register;

architecture behaviour of mips_bit_register is
begin
	reg_behav: process(in_val,clock)
   begin
      if (clock = '1') then
         out_val <= in_val after 5 ns;
      end if;
   end process reg_behav;

end architecture behaviour;

use work.dlx_types.all; 

entity mux is
     port (input_1,input_0 : in dlx_word; which: in bit; output: out dlx_word);
end entity mux;

architecture behaviour of mux is

begin
   mux_behav : process(input_1,input_0,which) is
      variable value : dlx_word := B"00000000000000000000000000000000";
   begin
      if (which = '1') then
         value := input_1;
      else
         value := input_0;
      end if;
      output <= value after 5 ns;
   end process mux_behav;

end architecture behaviour;

use work.dlx_types.all;

entity index_mux is
     port (input_1,input_0 : in register_index; which: in bit; output: out register_index);
end entity index_mux;

architecture behaviour of index_mux is

begin
   mux_behav : process(input_1,input_0,which) is
   begin
      if (which = '1') then
         output <= input_1 after 5 ns;
      else
         output <= input_0 after 5 ns;
      end if;
   end process mux_behav;

end architecture behaviour;

use work.dlx_types.all;

entity sign_extend is
     port (input: in half_word; signed: in bit; output: out dlx_word);
end entity sign_extend;

architecture behaviour of sign_extend is

begin
   sign_behav : process(input) is
      variable value : dlx_word := B"00000000000000000000000000000000";
   begin
      if (input(15) = '1') then
         value := "1111111111111111" & input;
      else
         value := "0000000000000000" & input;
      end if;
      output <= value after 5 ns;
   end process sign_behav;

end architecture behaviour;

use work.dlx_types.all; 
use work.bv_arithmetic.all; 

entity add4 is
    port (input: in dlx_word; output: out dlx_word);
end entity add4;

architecture behaviour of add4 is

begin
   add_four : process(input) is
      variable value : dlx_word := B"00000000000000000000000000000000";
      variable adding : dlx_word := B"00000000000000000000000000000100";
      variable overflow_flag : boolean;
   begin
      bv_addu(input, adding, value, overflow_flag);
      output <= value after 5 ns;
   end process add_four;

end architecture behaviour;
  
use work.dlx_types.all;
use work.bv_arithmetic.all;  

entity regfile is
     port (read_notwrite,clock : in bit; 
           regA,regB: in register_index; 
	   data_in: in  dlx_word; 
	   dataA_out,dataB_out: out dlx_word
	   );
end entity regfile; 

architecture behaviour of regfile is
   type register_type is array (0 to 31) of dlx_word;

begin
   reg_file : process(read_notwrite, clock, regA, regB, data_in) is
      variable reg: register_type;
   begin
      if (clock = '1') then
         if (read_notwrite = '1') then
            dataA_out <= reg(bv_to_integer(regA)) after 5 ns;
            dataB_out <= reg(bv_to_integer(regB)) after 5 ns;
         else
            reg(bv_to_integer(regA)) := data_in;
         end if;
      end if;
   end process reg_file;
end architecture behaviour;

use work.dlx_types.all;
use work.bv_arithmetic.all;

entity DM is
  
  port (
    address : in dlx_word;
    readnotwrite: in bit; 
    data_out : out dlx_word;
    data_in: in dlx_word; 
    clock: in bit); 
end DM;

architecture behaviour of DM is

begin  -- behaviour

  DM_behav: process(address,clock) is
    type memtype is array (0 to 1024) of dlx_word;
    variable data_memory : memtype;
  begin
    -- fill this in by hand to put some values in there
    data_memory(1023) := B"00000101010101010101010101010101";
    data_memory(0) := B"00000000000000000000000000000001";
    data_memory(1) := B"00000000000000000000000000000010";
    if clock'event and clock = '1' then
      if readnotwrite = '1' then
        -- do a read
        data_out <= data_memory(bv_to_natural(address)/4);
      else
        -- do a write
        data_memory(bv_to_natural(address)/4) := data_in; 
      end if;
    end if;


  end process DM_behav; 

end behaviour;

use work.dlx_types.all;
use work.bv_arithmetic.all;

entity IM is
  
  port (
    address : in dlx_word;
    instruction : out dlx_word;
    clock: in bit); 
end IM;

architecture behaviour of IM is

begin  -- behaviour

  IM_behav: process(address,clock) is
    type memtype is array (0 to 1024) of dlx_word;
    variable instr_memory : memtype;                   
  begin
    -- fill this in by hand to put some values in there
    -- first instr is 'LW R1,4092(R0)' 
    instr_memory(0) := B"10001100000000010000111111111100";
    -- next instr is 'ADD R2,R1,R1'
    instr_memory(1) := B"00000000001000010001000000100000";
    -- next instr is SW R2,8(R0)'
    instr_memory(2) := B"10101100000000100000000000001000";
    -- next instr is LW R3,8(R0)'
    instr_memory(3) := B"10001100000000110000000000001000"; 
    if clock'event and clock = '1' then
        -- do a read
        instruction <= instr_memory(bv_to_natural(address)/4);
    end if;
  end process IM_behav; 

end behaviour;







