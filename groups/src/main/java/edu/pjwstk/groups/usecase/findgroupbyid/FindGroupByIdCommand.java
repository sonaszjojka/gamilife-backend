package edu.pjwstk.groups.usecase.findgroupbyid;

import edu.pjwstk.core.Command;
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
