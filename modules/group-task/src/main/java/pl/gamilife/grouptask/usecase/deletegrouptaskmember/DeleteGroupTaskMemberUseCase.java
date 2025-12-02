package pl.gamilife.grouptask.usecase.deletegrouptaskmember;

import java.util.UUID;

public interface DeleteGroupTaskMemberUseCase {

    void execute (UUID groupTaskMemberId);
}
