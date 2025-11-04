package edu.pjwstk.grouptasks.usecase.deletegrouptaskmember;

import java.util.UUID;

public interface DeleteGroupTaskMemberUseCase {

    void execute (UUID groupTaskMemberId);
}
