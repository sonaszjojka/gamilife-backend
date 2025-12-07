package pl.gamilife.auth.usecase.getauthuser;

import pl.gamilife.shared.kernel.architecture.Command;

public record GetAuthenticatedUserCommand() implements Command {
    @Override
    public void validate() {

    }
}
