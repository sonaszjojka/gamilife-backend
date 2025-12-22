package pl.gamilife.auth.application.registeruser;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.io.Serializable;
import java.time.LocalDate;

public record RegisterUserCommand(

        @NotBlank
        String firstName,

        @NotBlank
        String lastName,

        @NotBlank
        @Email
        String email,

        @NotBlank
        String password,

        @NotBlank
        String username,

        @NotNull
        LocalDate dateOfBirth,

        boolean sendBudgetReports,

        boolean isProfilePublic
) implements Serializable, Command {
}
