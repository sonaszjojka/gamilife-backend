package pl.gamilife.user.usecase.getusers;

import pl.gamilife.shared.kernel.architecture.Command;

public record GetUsersCommand(
        String username,
        int page,
        int size
) implements Command {
    @Override
    public void validate() {

    }
}
