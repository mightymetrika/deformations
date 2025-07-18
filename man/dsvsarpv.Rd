% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dsvsarpv.R
\name{dsvsarpv}
\alias{dsvsarpv}
\title{Deformation of Sphere's Volume and Surface Area to Regular Polyhedron Volume}
\usage{
dsvsarpv(n, r = 1)
}
\arguments{
\item{n}{Integer vector specifying Platonic solid by number of faces.
Must be one of: 4 (tetrahedron), 6 (cube), 8 (octahedron),
12 (dodecahedron), 20 (icosahedron).}

\item{r}{Numeric vector of sphere radius/radii (must be positive). Default is 1.}
}
\value{
Numeric vector of volume deformity values
}
\description{
Calculates the volume discrepancy when transforming a sphere into a
Platonic solid. This measures the difference between the volume of a
polyhedron derived from volume-conserving edge length versus the volume
derived from surface-area-conserving edge length. This completes the 3D
deformity trilogy alongside dsvsarps and dsvsarpa.
}
\details{
Calculate Sphere-to-Platonic-Solid Volume Deformity

The function computes:
\deqn{d_{n,volume} = V_{volume} - V_{surface}}

Where:
\itemize{
\item \eqn{V_{volume} = \frac{4}{3}\pi r^3} (volume with volume-preserving edge length)
\item \eqn{V_{surface}} is the volume with surface-area-preserving edge length
}

For each Platonic solid with edge length a:
\itemize{
\item Tetrahedron (n=4): Volume = a³/(6√2)
\item Cube (n=6): Volume = a³
\item Octahedron (n=8): Volume = (√2/3)a³
\item Dodecahedron (n=12): Volume = a³(15+7√5)/4
\item Icosahedron (n=20): Volume = (5/12)(3+√5)a³
}

The volume deformity quantifies how the competing geometric constraints
(volume vs surface area conservation) affect the total enclosed volume
when transforming a sphere into a Platonic solid.
}
\examples{
# Basic usage
dsvsarpv(4)   # Tetrahedron volume deformity
dsvsarpv(6)   # Cube volume deformity

# All Platonic solids
dsvsarpv(c(4, 6, 8, 12, 20))

# Compare all three deformity types
solids <- c(4, 6, 8, 12, 20)
edge_def <- dsvsarps(solids)
area_def <- dsvsarpa(solids)
vol_def <- dsvsarpv(solids)

plot(solids, vol_def, col="green", pch=15,
     xlab="Faces", ylab="Deformity",
     main="3D Deformity Types")
points(solids, area_def, col="red", pch=16)
points(solids, edge_def, col="blue", pch=17)
legend("topright", c("Volume", "Surface Area", "Edge Length"),
       col=c("green","red","blue"), pch=c(15,16,17))

}
\seealso{
\code{\link{dsvsarps}} for 3D edge length deformity,
\code{\link{dsvsarpa}} for 3D surface area deformity
}
