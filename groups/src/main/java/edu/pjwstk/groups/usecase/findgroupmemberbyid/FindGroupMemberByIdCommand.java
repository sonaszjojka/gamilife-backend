package edu.pjwstk.groups.usecase.findgroupmemberbyid;

import edu.pjwstk.core.Command;
import jakarta.validation.ValidationException;

import java.util.UUID;

public record FindGroupMemberByIdCommand(UUID groupMemberId) implements Command {
    @Override
    public void validate() {
        if (groupMemberId == null) {
            throw new ValidationException("groupMemberId is required");
        }
    }
}
