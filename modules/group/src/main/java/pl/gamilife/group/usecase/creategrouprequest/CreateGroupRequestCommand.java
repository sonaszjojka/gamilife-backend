package pl.gamilife.group.usecase.creategrouprequest;

import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record CreateGroupRequestCommand(
        UUID groupId
) implements Command {
    @Override
    public void validate() {
        // Validation in API layer
    }
}
