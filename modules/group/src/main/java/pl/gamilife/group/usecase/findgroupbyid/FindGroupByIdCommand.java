package pl.gamilife.group.usecase.findgroupbyid;

import jakarta.validation.ValidationException;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record FindGroupByIdCommand(UUID groupId) implements Command {
    @Override
    public void validate() {
        if (groupId == null) {
            throw new ValidationException("groupId cannot be null");
        }
    }
}
