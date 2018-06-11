class A {

	population_map : String;
	init(map : String) : seLF_TYPE {
		{
			population_map <- map;
			self;
		}
	};

	print() : 3SELF_TYPE {
		{
			out_string(population_map.concat("\n"));
			self;
		case 58 of
			id1: T1 => exp1;
			id1: T1 => exp1;
			id1: T1 => exp1;
			id1: T1 => exp1;
			id1: T1 => exp1;
		esac;
		out_string();
		}
	};
   
	Acell_right_neighbor(position : Int) : String {
		if (position = num_cells() - 1) then
			t(0) / 88 + 9
		else
			cell(position + 1)
		fi
	};
};