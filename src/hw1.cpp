#include<iostream>
#include<vector>
#include<random>
#include<iomanip>
#include<cmath>
#include<stdexcept>
#include"hw1.h"

namespace algebra
{
    using Matrix = std::vector<std::vector<double>>;
    Matrix zeros(size_t n , size_t m )
    {
        Matrix arr(n , std::vector<double> (m, 0));
        return arr;
    }


    Matrix ones(size_t n , size_t m )
    {
        Matrix arr(n , std::vector<double> (m, 1));
        return arr;
    }

    Matrix random(size_t n , size_t m , double min , double max)
    {
        Matrix arr(n , std::vector<double> (m, 0));
        if(min >= max)
        {
            throw std::logic_error("max must be bigger than min");
        }
        for(size_t i{} ; i < n ; i++)
        {
            for(size_t j{} ; j < m ; j++)
            {
                std::random_device rd;
                std::mt19937 mt(rd());
                std::uniform_real_distribution<double> dist(min, max);
                arr[ i ][ j ] = dist(mt);
            }
        }

        return arr;
    }

    void show (const Matrix& matrix)
    {
        size_t n{matrix.size()};
        if(n == 0)
        {
            std::cout <<  "empty matrix \n[ ]" << std::endl;
        }
        else
        {
            size_t m{matrix[ 0 ].size()};

            for(size_t i{} ; i < n ; i++)
            {
                for(size_t j{} ; j < m ; j++)
                {
                    std::cout << std::setprecision(3) << std::fixed << matrix[ i ][ j ] << "\t \t" ;
                }

                std::cout << '\n' << std::endl;
            }   
        } 
    }

    Matrix multiply(const Matrix& matrix , double c)
    {
        size_t n{matrix.size()};
        if(n == 0)
        {
            std::cout << "empty matrix" << std::endl;
            return matrix; 
        }
        size_t m{matrix[ 0 ].size()};
        Matrix arr{ zeros(n , m) };


        for(size_t i{} ; i < n ; i++)
        {
            for(size_t j{} ; j < m ; j++)
            {
                arr[ i ][ j ] = matrix[ i ][ j ] * c;
            }

        } 
        return arr;
    }

    Matrix multiply(const Matrix& matrix1 , const Matrix& matrix2)
    {
        size_t n{ matrix1.size() };   
        size_t p{ matrix2.size() };
        if(n == 0)
        {
            return matrix1;
        }

        if(p == 0)
        {
            return matrix2;
        }

        size_t q{ matrix2[ 0 ].size() };
        size_t m{ matrix1[ 0 ].size() }; 

        if(m != p)
        {
            throw std::logic_error("invalid diminsions for multiplication");
        }
        else
        {
            Matrix arr{ zeros(n , q) };

            for(size_t i{} ; i < n ; i++)
            {
                for(size_t j{} ; j < q ; j++)
                {
                    double sum{};
                    for(size_t k{} ; k < p ; k++)
                    {
                        sum += (matrix1[ i ] [ k ] * matrix2[ k ][ j ]);
                    }
                    arr[ i ][ j ] = sum;

                }
            } 
            return arr;
        }
    }

    Matrix sum(const Matrix& matrix, double c)
    {
        size_t n { matrix.size() };
        if(n == 0)
        {
            return matrix ;
        }
        size_t m { matrix[0].size() };
        Matrix arr{zeros(n , m)};

        for(size_t i{} ; i < n ; i++)
        {
            for(size_t j{} ; j < m ; j++)
            {
                arr[ i ][ j ] = matrix[ i ][ j ] + c;
            }
        }

        return arr;
    }

    Matrix sum(const Matrix& matrix1, const Matrix& matrix2)
    {
        size_t n{ matrix1.size() };    
        size_t p{ matrix2.size() };
        if (n == 0 && p == 0) 
        {
            return Matrix {};
        }

        if(n == 0)
        {
            throw std::logic_error("one on the entries is an empty matrix");
        }

        if(p == 0)
        {
            throw std::logic_error("one on the entries is an empty matrix");
        }

        size_t q{ matrix2[ 0 ].size() };
        size_t m{ matrix1[ 0 ].size() };

        if (n == p && m == q)
        {
            Matrix arr{ zeros(n , m) };

            for(size_t i{} ; i < n ; i++)
            {
                for(size_t j{} ; j < m ; j++)
                {
                    arr[ i ][ j ] = matrix1[ i ][ j ] + matrix2[ i ][ j ];
                }
            }
            return arr;
        } 

        else
        {
            throw std::logic_error("invalid diminsions for summation");
        }
    }

    Matrix transpose(const Matrix& matrix)
    {
        size_t n{ matrix.size() };
        if(n == 0)
        {
            return matrix;
        }
        size_t m{ matrix[ 0 ].size() };   
        Matrix arr{ zeros(m , n) };

        for(size_t i{} ; i < n ; i++)
        {
            for(size_t j{} ; j < m ; j++)
            {
                arr[ j ][ i ] = matrix[ i ][ j ];
            }

        } 
        return arr;

    }

    Matrix minor(const Matrix& matrix, size_t n, size_t m)
    {
        size_t p{ matrix.size() };
        if(p == 0)
        {
            throw std::logic_error("entry is empty matrix");
        }
        size_t q{ matrix[ 0 ].size() };

        if(n > p || m > q)
        {
            throw std::logic_error(" its imposible to create a minor version of your matrix caused by max index exceed");
        }

        else
        {
            Matrix arr{zeros(p-1 , q-1)};
            for(size_t i{} ; i < p-1 ; i++)
            {
                for(size_t j{} ; j < q-1 ; j++)
                {
                    if(i < n && j < m)
                    {
                        arr[ i ][ j ] = matrix[ i ][ j ];
                    }

                    else if(i >= n && j < m)
                    {
                        arr[ i ][ j ] = matrix[ i+1 ][ j ];                    
                    }

                    else if(i < n && j >= m)
                    {
                        arr[ i ][ j ] = matrix[ i ][ j+1 ];                   
                    }

                    else if(i >= n && j >= m)
                    {
                        arr[ i ][ j ] = matrix[ i+1 ][ j+1 ];                    
                    }
                }
            }
            return arr;

        }
    }

    double determinant(const Matrix& matrix)
    {
        size_t n{ matrix.size() };
        if(n == 0)
        {
            return 1.0;
        }
        size_t m{ matrix[ 0 ].size() };   
        if(n == 2 && m == 2 )
        {
            double sumf{};
            sumf = (matrix[ 0 ][ 0 ] * matrix[ 1 ][ 1 ]) - (matrix[ 0 ][ 1 ] * matrix[ 1 ][ 0 ]);
            return sumf;
        }

        if(n != m)
        {
            throw std::logic_error("diminsions must be equal"); 
        }

        else
        {
            int j{1};
            double sum{};
            for(size_t i{} ; i < m ; i++)
            {
                sum += j * matrix[ 0 ][ i ] * determinant(minor(matrix , 0 , i));
                j = -j ; 
            }

            return sum ;
        }
    }

    Matrix inverse(const Matrix& matrix)
    {
        size_t n{ matrix.size() };
        if(n == 0)
        {
            return matrix;
        }
        size_t m{ matrix[ 0 ].size() };  

        if(n != m)
        {
            throw std::logic_error("diminsions must be equal"); 
        }

        else if(determinant(matrix) == 0)
        {
            throw std::logic_error("determinant must not be 0"); 
        }

        else
        {
            Matrix arr{ zeros(n , m) };

            for(size_t i{} ; i < n ; i++)
            {
                for(size_t j{} ; j < m ; j++)
                {
                    double temp{ determinant(minor(matrix , i , j)) };
                    double det { determinant(matrix) };
                    temp = temp / det ;
                    arr[ j ][ i ] = (std::pow(-1 , i+j)) * temp ;

                }
            }

            return arr;
        }
    }

    Matrix concatenate(const Matrix& matrix1, const Matrix& matrix2, int axis)
    {
        size_t n{ matrix1.size() };   
        size_t p{ matrix2.size() };
        if(n == 0)
        {
            return matrix2;
        }
        if(p == 0)
        {
            return matrix1;
        }
        size_t q{ matrix2[ 0 ].size() };
        size_t m{ matrix1[ 0 ].size() }; 
        if(axis == 0)
        {
            Matrix arr{ zeros(n + p , m) };
            if(m == q)
            {
                for(size_t i{} ; i < n ; i++)
                {
                    for(size_t j{} ; j < m ; j++)
                    {
                        arr[ i ][ j ] = matrix1[ i ][ j ];
                    }

                }

                for(size_t i{} ; i < p ; i++)
                {
                    for(size_t j{} ; j < m ; j++)
                    {
                        arr[ i + n ][ j ] = matrix2[ i ][ j ];
                    }

                }

                return arr;
            }

            else
            {
                throw std::logic_error("concatenation couldnt be done"); 
            }
        }

        else if(axis == 1)
        {
            Matrix arr{ zeros(n , m + q) };
            if(n == p)
            {
                for(size_t i{} ; i < n ; i++)
                {
                    for(size_t j{} ; j < m ; j++)
                    {
                        arr[ i ][ j ] = matrix1[ i ][ j ];
                    }

                }

                for(size_t i{} ; i < n ; i++)
                {
                    for(size_t j{} ; j < q ; j++)
                    {
                        arr[ i ][ j + m ] = matrix2[ i ][ j ];
                    }

                }

                return arr;
            }

            else
            {
                throw std::logic_error("concatenation couldnt be done"); 
            }
        }

        else
        {
            throw std::logic_error("the axis entry is not correct");
        }
    }

    Matrix ero_swap(const Matrix& matrix, size_t r1, size_t r2)
    {
        size_t n{ matrix.size() };
        if(n == 0)
        {
            return matrix;
        }
        size_t m{ matrix[ 0 ].size() }; 
        Matrix arr{ zeros(n , m) };

        if(r1 < n && r2 < n)
        {
            Matrix arr{ matrix };

            for(size_t j{} ; j < m ; j++)
            {
                double temp{ arr[ r1 ][ j ]};
                arr[ r1 ][ j ] = arr[ r2 ][ j ];
                arr[ r2 ][ j ] = temp;

            }

            return arr;
        }

        else
        {
            throw std::logic_error("wrong indexing ");
        }
    }

    Matrix ero_multiply(const Matrix& matrix, size_t r, double c)
    {

        size_t n{ matrix.size() };
        if(n == 0)
        {
            return matrix;
        }
        size_t m{ matrix[ 0 ].size() }; 
        Matrix arr{ zeros(n , m) }; 

        if(r < n)
        {
            Matrix arr{ matrix };

            for(size_t j{} ; j < m ; j++)
            {
                arr[ r ][ j ] = arr[ r ][ j ] * c;
            }

            return arr;
        }

        else
        {
            throw std::logic_error("wrong indexing ");
        }   
    }

    Matrix ero_sum(const Matrix& matrix, size_t r1, double c, size_t r2)
    {
        size_t n{ matrix.size() }; 
        if(n == 0)
        {
            return matrix;
        }
        size_t m{ matrix[ 0 ].size() }; 
        Matrix arr{ ero_multiply(matrix , r1 , c) };
        if(c == 0)
        {
            return matrix;
        }

        if(r1 < n && r2 < n)
        {
            for(size_t j{} ; j < m ; j++)
            {
                arr[ r2 ][ j ] += arr[ r1 ][ j ];
            }

            arr = ero_multiply(arr , r1 , 1.0 / c );
            return arr;
        }

        else
        {
            throw std::logic_error("wrong indexing ");
        }   
    }

    Matrix upper_triangular(const Matrix& matrix)
    {
        size_t n{ matrix.size() };
        if(n == 0)
        {
            return matrix;
        }
        size_t m{ matrix[ 0 ].size() }; 
        Matrix arr{ matrix };
        double cte{};
        if( n == m )
        {
            for(size_t i{} ; i < n ; i++)
            {
                if(arr[ i ][ i ] == 0.0)
                {
                    for(size_t j{ 1 } ; j < n ; j++)
                    {
                        if(arr[ j ][ i ] != 0.0)
                        {
                            arr = ero_swap(arr , i , j);
                            break ; 
                        }
                    }
                }
                for(size_t k{ i + 1 } ; k < n ; k++)
                {
                    cte = -(arr[ k ][ i ] / arr[ i ][ i ]);
                    arr = ero_sum(arr , i , cte , k);
                }
            }
            return arr;

        }

        else 
        {
            throw std::logic_error("matrix has to be rectangular");
        }
    }    
}
