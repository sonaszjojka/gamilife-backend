package edu.pjwstk.grouptasks.usecase.deletegrouptask;

import java.util.UUID;

public interface DeleteGroupTaskUseCase {
    void execute(UUID groupTaskId);
}
