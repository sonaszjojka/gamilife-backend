package pl.gamilife.grouptask.usecase.editgrouptaskmember;


import java.util.UUID;

public interface EditGroupTaskMemberUseCase {

    EditGroupTaskMemberResponse execute(UUID groupTaskMemberId, EditGroupTaskMemberRequest request);
}
