package pl.gamilife.grouptask.usecase.creategrouptaskmember;

import java.util.UUID;

public interface CreateGroupTaskMemberUseCase {
    CreateGroupTaskMemberResponse execute(UUID groupTaskId, UUID groupId, CreateGroupTaskMemberRequest request);
}
