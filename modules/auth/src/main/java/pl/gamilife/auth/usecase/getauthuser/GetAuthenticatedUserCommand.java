package pl.gamilife.auth.usecase.getauthuser;

import pl.gamilife.infrastructure.core.architecture.Command;

public record GetAuthenticatedUserCommand() implements Command {
    @Override
    public void validate() {

    }
}
