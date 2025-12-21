package pl.gamilife.auth.application.login;

import pl.gamilife.shared.kernel.architecture.Command;

public record LoginUserCommand(
        String email,
        String password
) implements Command {
    @Override
    public void validate() {

    }
}
