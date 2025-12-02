package edu.pjwstk.grouptasks.usecase.creategrouptaskmember;

import java.util.UUID;

public interface CreateGroupTaskMemberUseCase {
    CreateGroupTaskMemberResponse execute( UUID groupTaskId, CreateGroupTaskMemberRequest request);
}
