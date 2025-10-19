package edu.pjwstk.groups.usecase.createchatmessage;

import java.util.UUID;

public interface CreateChatMessageUseCase {
    CreateChatMessageResponse execute(CreateChatMessageRequest request, UUID groupId, UUID groupMemberId);
}
