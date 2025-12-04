package pl.gamilife.group.usecase.findallgroupsbyuserIdwhereuserismemberusecase;

import jakarta.validation.ValidationException;
import pl.gamilife.infrastructure.core.architecture.Command;

import java.util.UUID;

public record FindAllGroupsByUserIdWhereUserIsMemberCommand(
        UUID userId,
        Integer page,
        Integer size,
        String joinCode,
        Integer groupType,
        String groupName
) implements Command {
    @Override
    public void validate() {
        if (userId == null) {
            throw new ValidationException("userId cannot be null");
        }

        if (page == null) {
            throw new ValidationException("page cannot be null");
        }

        if (size == null) {
            throw new ValidationException("size cannot be null");
        }
    }
}
