#include <iostream>
#include<cmath>
using namespace std;

class Matrix
{
public:
	int rows, cols;
	double **matrix;
	bool q;
	int  *arr; // basic variables
	Matrix()
	{
		rows = 0, cols = 0;
	}
	Matrix(int n, int m)
	{
		rows = n, cols = m;
		arr = new int[m - 1];
		for (int i = 0; i < cols - 1; i++)
			arr[i] = 0;
		matrix = new double*[rows];
		for (int i = 0; i <rows; i++)
			matrix[i] = new double[cols];

	}
	void Input()
	{
		for (int i = 0; i < rows; i++)
		for (int j = 0; j < cols; j++)
			cin >> matrix[i][j];
	}
	bool check_Matrix_define(Matrix a, Matrix b)
	{
		if ((a.cols == b.cols) && (a.rows == b.rows))
			return 1;
		else
			return 0;
	}
	void fix_matrix(Matrix a)
	{
		for (int i = 0; i < a.rows; i++)
		{
			for (int j = 0; j < a.cols; j++)
			if (abs(a.matrix[i][j]) < 1e-6)
				a.matrix[i][j] = 0;
		}
	}

	void display_matrix()
	{
		for (int i = 0; i < rows; i++)
		{
			for (int j = 0; j < cols; j++)
			{
				cout << matrix[i][j] << " ";
			}
			cout << endl;
		}
		cout << endl;
	}

	Matrix echelon_form(Matrix a)
	{
		Matrix c(a.rows, a.cols), b(a.rows, a.cols);
		c = a;
		double lead, n;

		for (int i = 0; i<a.rows - 1; i++)
		{
			if (a.matrix[i][i] != 0)
			{
				lead = a.matrix[i][i];
				for (int x = i; x<c.rows - 1; x++)
				{
					n = a.matrix[x + 1][i];
					if (n != 0)
					{
						for (int j = 0; j<c.cols; j++)

							c.matrix[x + 1][j] += (a.matrix[i][j] / lead)*-n;
					}
				}
			}
			else if (a.matrix[i][i] == 0)
			{
				int first = i, second = i + 1;
				for (int j = i; j<a.rows; j++)
				if (a.matrix[j][i] != 0)
				{
					second = j;
				}
				for (int k = 0; k < a.cols; k++)
				{
					swap(c.matrix[first][k], c.matrix[second][k]);
				}
				lead = a.matrix[i][i];
				for (int x = i; x<c.rows - 1; x++)
				{
					n = a.matrix[x + 1][i];
					if (n != 0)
					{
						for (int j = 0; j<c.cols; j++)
							c.matrix[x + 1][j] += (a.matrix[i][j] / lead)*-n;

					}
				}


			}
			for (int k = 0; k < a.rows; k++)
			{
				if (c.matrix[i][k] == -0)
					c.matrix[i][k] = 0;
			}
		}

		fix_matrix(c);
		return c;
	}
	void reduced_echelon_form(Matrix a)
	{
		Matrix c(a.rows, a.cols), b(a.rows, a.cols);
		c = a;
		a.echelon_form(c);
		int pos = -1;
		double lead, n;
		for (int i = 0; i < a.rows; i++)
		{
			for (int j = 0; j < a.cols; j++)
			{
				if (a.matrix[i][j] != 0)
				{
					lead = a.matrix[i][j];
					pos = j;
					arr[j] = j;
					break;
				}
			}
			if (pos == -1)
				continue;
			if (i == 0)
			{
				for (int j = 0; j < c.cols; j++)
					c.matrix[i][j] /= lead;
			}
			else
			{
				for (int j = 0; j <c.cols; j++)
					c.matrix[i][j] /= lead;
				for (int x = i - 1; x >= 0; x--)
				{
					n = c.matrix[x][pos];
					for (int j = pos; j <a.cols; j++)
						c.matrix[x][j] += c.matrix[i][j] * -n;


				}
			}
			for (int k = 0; k < a.cols; k++)
			{
				if (c.matrix[i][k] == -0)
					c.matrix[i][k] = 0;
			}
		}
		fix_matrix(c);
		c.display_matrix();
	}
	Matrix Transpose(Matrix a)
	{
		Matrix trans(a.cols, a.rows);
		for (int i = 0; i < cols; i++)
		{
			for (int j = 0; j < rows; j++)
			{
				trans.matrix[i][j] = a.matrix[j][i];
			}
		}
		/*swap(rows, cols);
		for (int i = 0; i < rows; i++)
		{
		for (int j = 0; j < cols; j++)
		{
		matrix[i][j] = trans[i][j];
		}
		}*/
		return trans;
	}

};
Matrix multiply_by_scaler(Matrix a, double n)
{
	for (int i = 0; i < a.rows; i++)
	for (int j = 0; j < a.cols; j++)
		a.matrix[i][j] = n*a.matrix[i][j];
	return a;
}
double determinant_matrix(Matrix a)
{
	double det = 1;
	a.echelon_form(a);
	if (a.cols == a.rows)
	{
		for (int i = 0; i < a.rows; i++)
		{

			det *= a.matrix[i][i];
		}
	}
	else
	{
		cout << "Determinant can only be calculated to square matrix" << endl;
		det = 999999999;
	}
	return det;
}
Matrix inverse_matrix(Matrix a)
{
	if (determinant_matrix(a) == 0)
		cout << "The Matrix is not invertible\n";
	else
	{
		Matrix c(a.rows, a.cols);
		if (a.rows == 2 && a.cols == 2)
		{

			double A, B;
			A = a.matrix[0][0] * a.matrix[1][1];
			B = a.matrix[0][1] * a.matrix[1][0];
			c.matrix[0][0] = a.matrix[1][1];
			c.matrix[0][1] = -a.matrix[0][1];
			c.matrix[1][0] = -a.matrix[1][0];
			c.matrix[1][1] = a.matrix[0][0];
			c = multiply_by_scaler(c, 1 / (A - B));
			return c;

		}
	}

}
void consistent_or_inconsistent(Matrix a)
{
	Matrix c = a;
	c.reduced_echelon_form(a);
	c.q = false; // is inconsistent
	if (c.matrix[a.rows - 1][a.cols - 1] == 0)
		c.q = true;
	else

	{
		for (int i = 0; i < a.cols - 1; i++)
		{
			if (c.matrix[a.rows - 1][i] != 0)
				c.q = true;
		}
	}
	if (c.q)
		cout << "The system is consistent\n";
	else
		cout << "The system is inconsistent\n";
}
void sum_matrix(Matrix a, Matrix b)
{
	Matrix c(a.rows, a.cols);
	if (c.check_Matrix_define(a, b))
	{
		for (int i = 0; i < a.rows; i++)
		{
			for (int j = 0; j < a.cols; j++)
			{
				c.matrix[i][j] = a.matrix[i][j] + b.matrix[i][j];
			}
		}
		c.display_matrix();
	}
	else
	{
		cout << " A + B is not defined Because A and B have different sizes." << endl;

	}

}
void sub_matrix(Matrix a, Matrix b)
{

	Matrix c(a.rows, a.cols);
	if (c.check_Matrix_define(a, b))
	{
		for (int i = 0; i < a.rows; i++)
		{
			for (int j = 0; j < a.cols; j++)
			{
				c.matrix[i][j] = a.matrix[i][j] - b.matrix[i][j];
			}
		}
		c.display_matrix();
	}
	else
	{
		cout << " A + B is not defined Because A and B have different sizes." << endl;
	}


}
Matrix multiply_matrix(Matrix a, Matrix b)
{
	Matrix c(a.rows, a.cols);
	if (a.cols == b.rows)
	{
		c.rows = a.rows;
		c.cols = b.cols;
		for (int i = 0; i < a.rows; i++)
		{
			for (int j = 0; j < b.cols; j++)
			{
				c.matrix[i][j] = { 0 };
				for (int k = 0; k < a.cols; k++)
					c.matrix[i][j] += a.matrix[i][k] * b.matrix[k][j];
			}
		}
		c.q = true;
	}
	else
	{
		c.q = false;
	}
	return c;
}
void power_matrix(Matrix a, int x)
{
	Matrix c(a.rows, a.cols);
	if (x == 0)
	{
		for (int i = 0; i < a.rows; i++)
		for (int j = 0; j < a.cols; j++)
			c.matrix[i][j] = 0;
		for (int i = 0; i < a.rows; i++)
		{
			c.matrix[i][i] = 1;
		}
		c.display_matrix();
	}
	else
	{
		c = multiply_matrix(a, a);
		for (int i = 1; i < x - 1; i++)
		{
			c = multiply_matrix(c, a);
			c.display_matrix();
		}
	}


}
Matrix Input_Matrix_B()
{
	Matrix b;
	int n, m;
	cout << "input N , M of matrix B " << endl;
	cin >> n >> m;
	b = Matrix(n, m);
	cout << "input matrix " << endl;
	b.Input();
	return b;
}
void Display(Matrix a, Matrix b)
{
	Matrix c;
	int x;
	bool input_b = true;
	while (true)
	{
		cout << "\n		==========================================";
		cout << "\n		  1. sum two matrix";
		cout << "\n		  2. display matrix";
		cout << "\n		  3. substraction two matrix";
		cout << "\n		  4. multiplay two matrix";
		cout << "\n		  5. power of matrix";
		cout << "\n		  6. echelon form  of the matrix";
		cout << "\n		  7. transpose matrix";
		cout << "\n		  8. multiply matrix by scaler";
		cout << "\n		  9. calculate Determinant of a matrix";
		cout << "\n		  10. reduced echelon form";
		cout << "\n		  11. inverse matrix";
		cout << "\n		  12. Check consistent_or_inconsistent";
		cout << "\n		  13. Exit Program";
		cout << "\n		==========================================";
		cout << "\n\n";
		cin >> x;

		if (x == 1)
		{
			if (input_b)
			{
				b = Input_Matrix_B();
				input_b = false;
			}
			sum_matrix(a, b);
			cout << "========" << endl;
		}
		if (x == 2)
		{
			char o;
			cout << "select matrix A or B" << endl;
			cin >> o;
			if ((o == 'a') || (o == 'A'))
				a.display_matrix();
			else if (o == 'b' || o == 'B')
			{
				if (input_b)
				{
					b = Input_Matrix_B();
					input_b = false;
				}
				b.display_matrix();
			}
		}
		else if (x == 3)
		{
			if (input_b)
			{
				b = Input_Matrix_B();
				input_b = false;
			}
			sub_matrix(a, b);
			cout << "========" << endl;
		}
		else if (x == 4)
		{
			if (input_b)
			{
				b = Input_Matrix_B();
				input_b = false;
			}
			c = multiply_matrix(a, b);
			if (c.q)
			{
				c.display_matrix();
			}
			else
			{
				cout << " A * B is not defined Because cols of A not equal rows of B ." << endl;
			}
			cout << "========" << endl;
		}
		else if (x == 5)
		{
			int p = 1;
			char o;
			cout << "select matrix A or B" << endl;
			cin >> o;
			if ((o == 'a') || (o == 'A'))
			{
				if (a.cols == a.rows)
				{
					cout << "input num of power  : ";
					cin >> p;
					power_matrix(a, p);
				}
				else
					cout << " Matrix power is not defined Because cols of A not equal rows of A ." << endl;



			}
			if ((o == 'b') || (o == 'B'))
			{
				if (input_b)
				{
					b = Input_Matrix_B();
					input_b = false;
				}
				if (b.cols == b.rows)
				{
					cout << "input num of power  : ";
					cin >> p;
					power_matrix(b, p);
				}
				else
					cout << " Matrix power is not defined Because cols of B not equal rows of B ." << endl;

			}

			cout << "========" << endl;
		}
		else if (x == 6)
		{

			char o;
			cout << "select matrix A or B" << endl;;
			cin >> o;
			if ((o == 'a') || (o == 'A'))
			{
				c = a.echelon_form(a);
				cout << "Echelon form of the matrix" << endl;
				c.display_matrix();
			}
			else if (o == 'b' || o == 'B')
			{
				if (input_b)
				{
					b = Input_Matrix_B();
					input_b = false;
				}
				c = b.echelon_form(b);
				cout << "Echelon form of the matrix" << endl;
				c.display_matrix();
			}
			cout << "========" << endl;
		}
		else if (x == 7)
		{
			Matrix trans;
			char o;
			cout << "select matrix A or B" << endl;;
			cin >> o;
			if ((o == 'a') || (o == 'A'))
			{
				trans = a.Transpose(a);
				trans.display_matrix();
			}
			if ((o == 'b') || (o == 'B'))
			{
				if (input_b)
				{
					b = Input_Matrix_B();
					input_b = false;
				}
				cout << endl;
				trans = b.Transpose(b);
				trans.display_matrix();
			}
			cout << "========" << endl;
		}
		else if (x == 8)
		{
			int n;
			char o;
			cout << "select matrix A or B" << endl;;
			cin >> o;
			if ((o == 'a') || (o == 'A'))
			{
				cout << "input scaler" << endl;
				cin >> n;
				c = multiply_by_scaler(a, n);
			}
			else if (o == 'b' || o == 'B')
			{
				if (input_b)
				{
					b = Input_Matrix_B();
					input_b = false;
				}
				cout << "input scaler" << endl;
				cin >> n;
				c = multiply_by_scaler(b, n);
			}
			c.display_matrix();
			cout << "========" << endl;
		}
		else if (x == 9)
		{
			char o;
			double det;
			cout << "select matrix A or B" << endl;;
			cin >> o;
			if ((o == 'a') || (o == 'A'))
			{
				det = determinant_matrix(a);
			}
			if ((o == 'b') || (o == 'B'))
			{
				if (input_b)
				{
					b = Input_Matrix_B();
					input_b = false;
				}
				det = determinant_matrix(b);
			}
			if (det != 999999999)
				cout << det << endl;
			cout << "========" << endl;
		}
		else if (x == 10)
		{
			char o;
			cout << "select matrix A or B" << endl;;
			cin >> o;
			if ((o == 'a') || (o == 'A'))
			{
				a.reduced_echelon_form(a);
			}
			if ((o == 'b') || (o == 'B'))
			{
				if (input_b)
				{
					b = Input_Matrix_B();
					input_b = false;
				}
				b.reduced_echelon_form(b);

			}
			cout << "========" << endl;
		}
		else if (x == 11)
		{
			char o;
			cout << "select matrix A or B" << endl;;
			cin >> o;
			if ((o == 'a') || (o == 'A'))
			{
				c = inverse_matrix(a);
			}
			if ((o == 'b') || (o == 'B'))
			{
				if (input_b)
				{
					b = Input_Matrix_B();
					input_b = false;
				}
				c = inverse_matrix(b);
			}
			c.display_matrix();
			cout << "========" << endl;
		}
		else if (x == 12)
		{
			char o;
			cout << "select matrix A or B" << endl;;
			cin >> o;
			if ((o == 'a') || (o == 'A'))
			{
				consistent_or_inconsistent(a);
			}
			if ((o == 'b') || (o == 'B'))
			{
				if (input_b)
				{
					b = Input_Matrix_B();
					input_b = false;
				}
				consistent_or_inconsistent(b);

			}
			cout << "========" << endl;
		}
		else
			break;


	}
}

int main()
{
	Matrix a, b;
	int n, m;
	cout << "input N , M of matrix A " << endl;
	cin >> n >> m;
	a = Matrix(n, m);
	cout << "input matrix " << endl;
	a.Input();
	Display(a, b);

	return 0;
}