package pl.gamilife.grouptask.usecase.editgrouptaskmember;

import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;
import pl.gamilife.grouptask.entity.GroupTaskMember;
import pl.gamilife.grouptask.exception.domain.GroupTaskMemberNotFoundException;
import pl.gamilife.grouptask.repository.GroupTaskMemberRepository;

import java.util.UUID;

@Service
public class EditGroupTaskMemberUseCaseImpl implements EditGroupTaskMemberUseCase {

    private final GroupTaskMemberRepository groupTaskMemberRepository;
    private final EditGroupTaskMemberMapper editGroupTaskMemberMapper;

    public EditGroupTaskMemberUseCaseImpl(GroupTaskMemberRepository groupTaskMemberRepository, EditGroupTaskMemberMapper editGroupTaskMemberMapper) {
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
