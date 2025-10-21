package edu.pjwstk.grouptasks.usecase.creategrouptaskmember;

import edu.pjwstk.common.groupsApi.GroupApi;
import edu.pjwstk.common.groupsApi.exception.GroupMemberNotFoundException;
import edu.pjwstk.grouptasks.entity.GroupTask;
import edu.pjwstk.grouptasks.entity.GroupTaskMember;
import edu.pjwstk.grouptasks.exception.GroupTaskNotFoundException;
import edu.pjwstk.grouptasks.repository.GroupTaskMemberRepository;
import edu.pjwstk.grouptasks.repository.GroupTaskRepository;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
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
    public CreateGroupTaskMemberResponse execute( UUID groupTaskId, CreateGroupTaskMemberRequest request) {
        if (groupsProvider.findGroupMemberById(request.groupMemberId())==null)
        {
            throw new GroupMemberNotFoundException("Group member with id:" + request.groupMemberId() + " does not exist");
        }
        //TODO check if group member belongs to group associated with group task

        GroupTask groupTask = groupTaskRepository.findByGroupTaskId(groupTaskId).orElseThrow(
                () -> new GroupTaskNotFoundException("Group task with id:" + groupTaskId + " does not exist"));

        GroupTaskMember groupTaskMember = createGroupTaskMemberMapper.toEntity( groupTask, request.groupMemberId(), UUID.randomUUID());
        GroupTaskMember savedGroupTaskMember = groupTaskMemberRepository.save(groupTaskMember);
        return createGroupTaskMemberMapper.toResponse(savedGroupTaskMember);
    }
}
