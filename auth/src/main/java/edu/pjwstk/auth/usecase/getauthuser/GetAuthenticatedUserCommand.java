package edu.pjwstk.auth.usecase.getauthuser;

import edu.pjwstk.core.Command;

public record GetAuthenticatedUserCommand() implements Command {
    @Override
    public void validate() {

    }
}
