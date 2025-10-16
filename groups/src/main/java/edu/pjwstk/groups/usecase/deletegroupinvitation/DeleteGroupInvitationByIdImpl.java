package edu.pjwstk.groups.usecase.deletegroupinvitation;

import edu.pjwstk.groups.exception.GroupInvitationNotFoundException;
import edu.pjwstk.groups.repository.GroupInvitationRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
public class DeleteGroupInvitationByIdImpl implements DeleteGroupInvitationById{

    private final GroupInvitationRepository groupInvitationRepository;

    public DeleteGroupInvitationByIdImpl(GroupInvitationRepository groupInvitationRepository) {
        this.groupInvitationRepository = groupInvitationRepository;
    }

    @Override
    @Transactional
    public void execute(UUID groupInvitationId) {
        groupInvitationRepository.findById(groupInvitationId)
                        .orElseThrow(() -> new GroupInvitationNotFoundException("Group invitation with id:"
                                + groupInvitationId + " not found!"));
        groupInvitationRepository.deleteById(groupInvitationId);
    }
}
