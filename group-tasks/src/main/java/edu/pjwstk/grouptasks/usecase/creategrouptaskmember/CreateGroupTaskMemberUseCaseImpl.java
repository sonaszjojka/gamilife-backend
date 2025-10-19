package edu.pjwstk.grouptasks.usecase.creategrouptaskmember;

import edu.pjwstk.common.groupsApi.GroupApi;
import edu.pjwstk.grouptasks.entity.GroupTask;
import edu.pjwstk.grouptasks.entity.GroupTaskMember;
import edu.pjwstk.grouptasks.repository.GroupTaskMemberRepository;
import edu.pjwstk.grouptasks.repository.GroupTaskRepository;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class CreateGroupTaskMemberUseCaseImpl implements CreateGroupTaskMemberUseCase {
    private final GroupTaskMemberRepository groupTaskMemberRepository;
    private final CreateGroupTaskMemberMapper createGroupTaskMemberMapper;
    private final GroupApi groupsProvider;
    private final GroupTaskRepository groupTaskRepository;

    public CreateGroupTaskMemberUseCaseImpl(GroupTaskMemberRepository groupTaskMemberRepository,
                                            CreateGroupTaskMemberMapper createGroupTaskMemberMapper, GroupApi groupsProvider, GroupTaskRepository groupTaskRepository) {
        this.groupTaskMemberRepository = groupTaskMemberRepository;
        this.createGroupTaskMemberMapper = createGroupTaskMemberMapper;
        this.groupsProvider = groupsProvider;
        this.groupTaskRepository = groupTaskRepository;
    }


    @Override
    public CreateGroupTaskMemberResponse execute( UUID groupTaskId, int groupMemberId) {
        if (groupsProvider.findGroupMemberById(groupMemberId)==null)
        {
            throw new IllegalArgumentException("Group member with id:" + groupMemberId + " does not exist");
        }
        if (groupTaskRepository.findByGroupTaskId(groupTaskId)==null)
        {
            throw new IllegalArgumentException("Group task with id:" + groupTaskId + " does not exist");
        }
        GroupTask groupTask = groupTaskRepository.findByGroupTaskId(groupTaskId);
        GroupTaskMember groupTaskMember = createGroupTaskMemberMapper.toEntity( groupTask, groupMemberId, UUID.randomUUID());
        GroupTaskMember savedGroupTaskMember = groupTaskMemberRepository.save(groupTaskMember);
        return createGroupTaskMemberMapper.toResponse(savedGroupTaskMember);
    }
}
