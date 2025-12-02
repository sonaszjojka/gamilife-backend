package pl.gamilife.grouptask.usecase.creategrouptask;

import java.util.UUID;

public interface CreateGroupTaskUseCase {
    CreateGroupTaskResponse execute(CreateGroupTaskRequest request, UUID groupId);
}
