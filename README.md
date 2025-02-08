# Discrete event simulation of components of early psychosis care in England

This repository contains the discrete event simulation model developed to evaluate and prioritize components of early psychosis care in England. The model simulates the pathway through EIP services, incorporating service user engagement and lifetime outcomes.

## Project structure

-   `src/`: Source code for the simulation model

-   `tests/`: Unit tests and internal validation scripts

-   `data/`: Data used for calculating parameters

-   `docs/`: Model documentation

## Installation and setup

This project uses [renv](https://rstudio.github.io/renv/){.uri} to manage package dependencies.

1.  Clone the repository:

    ``` bash
    git clone https://github.com/edpenington/EIP-component-DES
    cd EIP-component-DES
    ```

2.  Open R in the project directory and run the setup script

  ``` R
  source("setup.R")
  ```
  
  This will install `renv` and install required packages.

When you pull updates that include new dependencies, run:

```R
renv::restore()
```

## Usage

To run the most basic version of the model:

```R
source("src/run_model.R")
```

A full user guide to the model is provided in `docs/user_guide.md`

## Testing

Run unit tests using:

```R
testthat::test_dir("tests/")
```

## Documentation

See `docs/technical_documentation.md` for detailed model structure, parameter list, validation procedures and additional technical details.

The protocol for developing this model was published prospectively here [Add link]

## License

This project is licensed under the MIT License - see `LICENSE` file

## Citation

If you use this model in your research, please cite:
[Citation details to be added after publication]
