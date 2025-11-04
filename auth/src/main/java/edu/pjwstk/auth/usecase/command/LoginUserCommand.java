package edu.pjwstk.auth.usecase.command;

import edu.pjwstk.core.Command;

public record LoginUserCommand(
        String email,
        String password
) implements Command {
}
