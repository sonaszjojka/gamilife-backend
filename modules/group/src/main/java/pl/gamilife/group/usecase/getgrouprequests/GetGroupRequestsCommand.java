package pl.gamilife.group.usecase.getgrouprequests;

import pl.gamilife.shared.kernel.architecture.Command;

import java.io.Serializable;
import java.util.UUID;

public record GetGroupRequestsCommand(
        UUID groupId,
        Integer statusId,
        Integer page,
        Integer size
) implements Command, Serializable {
    @Override
    public void validate() {
        if (groupId == null) {
            throw new IllegalArgumentException("Group ID cannot be null");
        }
    }
}
