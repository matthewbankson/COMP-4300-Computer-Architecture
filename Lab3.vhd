library work;
use work.bv_arithmetic.all;
use work.dlx_types.all;

entity dlx_register is
	port(in_val: in dlx_word; clock: in bit; out_val: out dlx_word);
end entity dlx_register;

architecture behaviour of dlx_register is

begin
	reg_behav : process(in_val,clock) is
		variable value : dlx_word := B"00000000000000000000000000000000";
	begin
		if (clock = '1') then
			value := in_val;
		end if;
		out_val <= value after 5 ns;
	end process reg_behav;

end architecture behaviour;

use work.dlx_types.all;

entity mux is
	port(input_1,input_0 : in dlx_word; which : in bit; output : out dlx_word);
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

entity sign_extend is
	port(input : in half_word; output: out dlx_word);
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
	port(input: in dlx_word; output: out dlx_word);
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
	port(read_notwrite,clock : in bit; regA,regB : in register_index; data_in : in dlx_word;
		dataA_out,dataB_out : out dlx_word);
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