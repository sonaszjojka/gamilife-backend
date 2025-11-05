package edu.pjwstk.grouptasks.usecase.editgrouptaskmember;

import edu.pjwstk.api.groups.GroupApi;
import edu.pjwstk.grouptasks.entity.GroupTaskMember;
import edu.pjwstk.grouptasks.exception.domain.GroupTaskMemberNotFoundException;
import edu.pjwstk.grouptasks.repository.GroupTaskMemberRepository;
import edu.pjwstk.grouptasks.repository.GroupTaskRepository;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;

import java.util.UUID;
@Service
public class EditGroupTaskMemberUseCaseImpl implements EditGroupTaskMemberUseCase {

    private final GroupTaskMemberRepository groupTaskMemberRepository;
    private final EditGroupTaskMemberMapper editGroupTaskMemberMapper;
    public EditGroupTaskMemberUseCaseImpl(GroupTaskMemberRepository groupTaskMemberRepository, EditGroupTaskMemberMapper editGroupTaskMemberMapper, GroupApi groupApi, GroupTaskRepository groupTaskRepository) {
        this.groupTaskMemberRepository = groupTaskMemberRepository;
        this.editGroupTaskMemberMapper = editGroupTaskMemberMapper;
    }


    @Override
    @Transactional
    public EditGroupTaskMemberResponse execute(UUID groupTaskMemberId, EditGroupTaskMemberRequest request) {

        GroupTaskMember groupTaskMember = groupTaskMemberRepository.findByGroupTaskMemberId(groupTaskMemberId).orElseThrow(
                () -> new GroupTaskMemberNotFoundException("Group Task Member with id:" + groupTaskMemberId + " does not exist"));

        groupTaskMember.setIsMarkedDone(request.isMarkedDone());

        return editGroupTaskMemberMapper.toResponse(groupTaskMemberRepository.save(groupTaskMember));
    }
}
