package edu.pjwstk.auth.usecase.login;

import edu.pjwstk.core.Command;

public record LoginUserCommand(
        String email,
        String password
) implements Command {
    @Override
    public void validate() {

    }
}
