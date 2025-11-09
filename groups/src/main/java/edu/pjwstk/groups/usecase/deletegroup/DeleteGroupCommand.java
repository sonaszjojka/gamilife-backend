package edu.pjwstk.groups.usecase.deletegroup;

import edu.pjwstk.core.Command;
import jakarta.validation.ValidationException;

import java.util.UUID;

public record DeleteGroupCommand(UUID groupId) implements Command {
    @Override
    public void validate() {
        if (groupId == null) {
            throw new ValidationException("groupId cannot be null");
        }
    }
}
