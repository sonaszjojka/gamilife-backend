package pl.gamilife.group.usecase.findgroupmemberbyid;

import jakarta.validation.ValidationException;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record FindGroupMemberByIdCommand(UUID groupMemberId) implements Command {
    @Override
    public void validate() {
        if (groupMemberId == null) {
            throw new ValidationException("groupMemberId is required");
        }
    }
}
