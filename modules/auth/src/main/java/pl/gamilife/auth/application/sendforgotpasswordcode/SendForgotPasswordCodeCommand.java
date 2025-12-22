package pl.gamilife.auth.application.sendforgotpasswordcode;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import pl.gamilife.shared.kernel.architecture.Command;

public record SendForgotPasswordCodeCommand(@NotBlank @Email String email) implements Command {
}
