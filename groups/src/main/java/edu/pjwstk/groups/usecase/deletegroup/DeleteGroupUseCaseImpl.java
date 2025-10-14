package edu.pjwstk.groups.usecase.deletegroup;

import edu.pjwstk.common.groupsApi.exception.GroupNotFoundException;
import edu.pjwstk.groups.repository.GroupRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
public class DeleteGroupUseCaseImpl implements DeleteGroupUseCase {

    private GroupRepository groupRepository;

    public DeleteGroupUseCaseImpl(GroupRepository groupRepository) {
        this.groupRepository = groupRepository;
    }

    @Override
    @Transactional
    public void execute(UUID groupId) {
        groupRepository.findById(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id: " + groupId + " not found!"));

        //todo: Group Tasks + Group Shop etc. + Module + Finance?
        groupRepository.deleteById(groupId);
    }
}
