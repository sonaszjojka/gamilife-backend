package edu.pjwstk.groups.controller;

import edu.pjwstk.groups.controller.request.CreateChatMessageRequest;
import edu.pjwstk.groups.usecase.createchatmessage.CreateChatMessageCommand;
import edu.pjwstk.groups.usecase.createchatmessage.CreateChatMessageResult;
import edu.pjwstk.groups.usecase.createchatmessage.CreateChatMessageUseCase;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups/{groupId}/members/{groupMemberId}/chat-messages")
public class ChatMessageController {

    private final CreateChatMessageUseCase createChatMessageUseCase;

    public ChatMessageController(CreateChatMessageUseCase createChatMessageUseCase) {
        this.createChatMessageUseCase = createChatMessageUseCase;
    }

    @PostMapping
    public ResponseEntity<CreateChatMessageResult> save(@RequestBody @Valid CreateChatMessageRequest request,
                                                        @PathVariable("groupId") UUID groupId,
                                                        @PathVariable("groupMemberId") UUID groupMemberId) {
        CreateChatMessageResult response = createChatMessageUseCase.execute(new CreateChatMessageCommand(
                groupId,
                groupMemberId,
                request.content(),
                request.isImportant()
        ));
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

}