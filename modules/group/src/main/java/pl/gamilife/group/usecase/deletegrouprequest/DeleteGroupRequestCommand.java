package pl.gamilife.group.usecase.deletegrouprequest;

import jakarta.validation.ValidationException;
import pl.gamilife.infrastructure.core.architecture.Command;

import java.util.UUID;

public record DeleteGroupRequestCommand(
        UUID groupId,
        UUID groupRequestId
) implements Command {
    @Override
    public void validate() {
        if (groupId == null) {
            throw new ValidationException("groupId cannot be null");
        }

        if (groupRequestId == null) {
            throw new ValidationException("groupRequestId cannot be null");
        }
    }
}
