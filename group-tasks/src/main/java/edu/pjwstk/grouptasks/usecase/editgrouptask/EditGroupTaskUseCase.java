package edu.pjwstk.grouptasks.usecase.editgrouptask;

import java.util.UUID;

public interface EditGroupTaskUseCase {
    EditGroupTaskResponse execute(UUID GroupTaskId, EditGroupTaskRequest req);
}
