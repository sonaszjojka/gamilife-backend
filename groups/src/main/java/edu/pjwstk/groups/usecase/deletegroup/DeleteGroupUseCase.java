package edu.pjwstk.groups.usecase.deletegroup;

import java.util.UUID;

public interface DeleteGroupUseCase {
    void execute(UUID groupId);
}
