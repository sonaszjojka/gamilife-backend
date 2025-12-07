package pl.gamilife.group.usecase.getgroups.getbyid;

import pl.gamilife.shared.kernel.architecture.Command;

import java.io.Serializable;
import java.util.UUID;

public record GetGroupByIdCommand(
        UUID groupId,
        Boolean isForLoggedUser) implements Command, Serializable {
    @Override
    public void validate() {

    }
}
