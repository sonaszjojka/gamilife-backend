package edu.pjwstk.groups.usecase.findgroupbyid;

import pl.gamilife.infrastructure.core.architecture.Command;
import jakarta.validation.ValidationException;

import java.util.UUID;

public record FindGroupByIdCommand(UUID groupId) implements Command {
    @Override
    public void validate() {
        if (groupId == null) {
            throw new ValidationException("groupId cannot be null");
        }
    }
}
