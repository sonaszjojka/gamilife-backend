package edu.pjwstk.grouptasks.usecase.creategrouptask;

import java.util.UUID;

public interface CreateGroupTaskUseCase {
    CreateGroupTaskResponse execute(CreateGroupTaskRequest request, UUID taskId);
}
